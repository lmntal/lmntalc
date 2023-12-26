use std::collections::HashMap;

use crate::{
    ast::{ASTNode, AtomName},
    data::{
        guard::{Guard, GuardNode, GuardSource, ProcessConstraint},
        id::AtomId,
        rule::Rule,
    },
    token::Operator,
};

pub(super) fn visit_guard(rule: &mut Rule, process_list: &ASTNode) -> Guard {
    let mut guards = Guard::default();
    let mut defined = HashMap::new();
    if let ASTNode::ProcessList { processes, .. } = process_list {
        for process in processes {
            if let ASTNode::Atom { name, args, .. } = process {
                match name {
                    // assignment
                    AtomName::Operator(Operator::Equal) => {
                        assert!(args.len() == 2);
                        let lhs = match &args[0] {
                            ASTNode::Link { name, .. } => name.to_string(),
                            _ => unreachable!(),
                        };
                        let rhs = &args[1];
                        let node = transform_guard_expr(rhs, &defined);
                        let id = rule.register_def(&lhs);
                        guards.add_definition(id, node);
                        defined.insert(lhs, id);
                    }
                    // comparison
                    AtomName::Operator(op) if op.is_relational() => {
                        let node = transform_guard_expr(process, &defined);
                        guards.add_constraint(node);
                    }
                    // type constraint
                    AtomName::Keyword(s) => {
                        let ty = ProcessConstraint::from(s.as_str());
                        let mut vars = Vec::new();
                        for arg in args {
                            if let ASTNode::Link { name, .. } = arg {
                                vars.push(GuardSource::Placeholder(name.clone()));
                            } else {
                                unimplemented!("type constraint on non-link")
                            }
                        }
                        guards.add_constraint(GuardNode::Func(ty, vars));
                    }
                    _ => {
                        panic!("illegal guard in rule {}", rule.name)
                    }
                }
            } else {
                panic!("illegal guard in rule {}", rule.name)
            }
        }
    } else {
        unreachable!("visit_guard called with non-process-list node")
    }
    guards
}

fn transform_guard_expr(expr: &ASTNode, defined: &HashMap<String, AtomId>) -> GuardNode {
    match expr {
        ASTNode::Atom { name, args, .. } => match name {
            AtomName::Int(i) => GuardNode::Int(*i),
            AtomName::Float(f) => GuardNode::Float(*f),
            AtomName::Operator(op) => {
                let lhs = &args[0];
                let lhs = transform_guard_expr(lhs, defined);
                let rhs = &args[1];
                let rhs = transform_guard_expr(rhs, defined);
                GuardNode::Binary(*op, Box::new(lhs), Box::new(rhs))
            }
            AtomName::Keyword(..) | AtomName::Char(..) | AtomName::Plain(..) => unreachable!(),
        },
        ASTNode::Link { name, .. } => {
            if let Some(id) = defined.get(name) {
                GuardNode::Var(GuardSource::Definition(*id))
            } else {
                GuardNode::Var(GuardSource::Placeholder(name.clone()))
            }
        }
        ASTNode::Context { .. } => {
            unimplemented!("context will be implemented in the future")
        }
        _ => {
            unreachable!()
        }
    }
}
