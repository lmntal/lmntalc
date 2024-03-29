use std::collections::HashMap;

use crate::{
    frontend::{ast::AtomName, token::Operator},
    model::{
        guard::{Guard, GuardNode, GuardSource, ProcessConstraint, VariableId, RESERVED_FUNC},
        rule::Rule,
    },
    FunctorName, Process, ProcessList,
};

pub(super) fn visit_guard(rule: &mut Rule, process_list: &ProcessList) -> Guard {
    let mut guards = Guard::default();
    let mut defined = HashMap::new();
    let mut id: usize = 0;
    for process in &process_list.processes {
        if let Process::Atom(atom) = process {
            match &atom.name.0 {
                // assignment
                AtomName::Operator(Operator::Equal) => {
                    assert!(atom.args.len() == 2);
                    let lhs = match &atom.args[0] {
                        Process::Link(link) => link.name.to_string(),
                        _ => unreachable!(),
                    };
                    let rhs = &atom.args[1];
                    let node = transform_guard_expr(rhs, &defined);
                    guards.add_definition(id.into(), &lhs, node);
                    defined.insert(lhs, id.into());
                    id += 1;
                }
                // comparison
                AtomName::Operator(op) if op.is_relational() => {
                    let node = transform_guard_expr(process, &defined);
                    guards.add_constraint(node);
                }
                // type constraint
                AtomName::Functor(FunctorName::AtomName(s)) => {
                    let ty = from_keyword(s);
                    let mut vars = Vec::new();
                    for arg in &atom.args {
                        if let Process::Link(link) = arg {
                            vars.push(GuardSource::Placeholder(link.name.clone()));
                        } else {
                            unimplemented!("type constraint on non-link")
                        }
                    }
                    guards.add_constraint(GuardNode::Constraint(ty, vars));
                }
                _ => {
                    panic!("illegal guard in rule {}", rule.name)
                }
            }
        } else {
            panic!("illegal guard in rule {}", rule.name)
        }
    }

    guards
}

fn transform_guard_expr(expr: &Process, defined: &HashMap<String, VariableId>) -> GuardNode {
    match expr {
        Process::Atom(atom) => match &atom.name.0 {
            AtomName::Int(i) => GuardNode::Int(*i),
            AtomName::Float(f) => GuardNode::Float(*f),
            AtomName::Operator(op) => {
                let lhs = &atom.args[0];
                let lhs = transform_guard_expr(lhs, defined);
                let rhs = &atom.args[1];
                let rhs = transform_guard_expr(rhs, defined);
                GuardNode::Binary(*op, Box::new(lhs), Box::new(rhs))
            }
            AtomName::Functor(name) => {
                if let Some(func) = RESERVED_FUNC
                    .iter()
                    .find(|func| func.name == name.to_string())
                {
                    let vars = atom
                        .args
                        .iter()
                        .map(|arg| transform_guard_expr(arg, defined))
                        .collect::<Vec<_>>();
                    GuardNode::Function(func.clone(), vars)
                } else {
                    panic!("cannot find function {}", name)
                }
            }
            AtomName::Char(..) => unreachable!(),
        },
        Process::Hyperlink(hyperlink) => {
            if let Some(id) = defined.get(&hyperlink.name.0) {
                GuardNode::Var(GuardSource::Variable(*id))
            } else {
                GuardNode::Var(GuardSource::Placeholder(format!("!{}", hyperlink.name.0)))
            }
        }
        Process::Link(link) => {
            if let Some(id) = defined.get(&link.name) {
                GuardNode::Var(GuardSource::Variable(*id))
            } else {
                GuardNode::Var(GuardSource::Placeholder(link.name.clone()))
            }
        }
        Process::ProcessContext(_) => {
            unimplemented!("context will be implemented in the future")
        }
        _ => {
            unreachable!()
        }
    }
}

fn from_keyword(s: &str) -> ProcessConstraint {
    match s {
        "int" => ProcessConstraint::Int,
        "float" => ProcessConstraint::Float,
        "hlink" => ProcessConstraint::Hyperlink,
        "ground" => ProcessConstraint::Ground,
        "unary" => ProcessConstraint::Unary,
        "uniq" => ProcessConstraint::Unique,
        _ => panic!("illegal type constraint"),
    }
}
