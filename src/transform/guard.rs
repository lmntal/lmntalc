use crate::{
    ast::ASTNode,
    data::{
        guard::{Guard, GuardNode, ProcessConstraint},
        rule::Rule,
        Process,
    },
};

use super::util::solve_name;

pub(super) fn visit_guard(rule: &mut Rule, process_list: &ASTNode) -> Guard {
    let mut guards = Guard::default();
    if let ASTNode::ProcessList { processes, .. } = process_list {
        for process in processes {
            if let ASTNode::Atom { name, args, .. } = process {
                match name.as_str() {
                    // assignment
                    "=" => {
                        assert!(args.len() == 2);
                        let lhs = match &args[0] {
                            ASTNode::Atom { name, .. } => name,
                            _ => unreachable!(),
                        };
                        let rhs = &args[1];
                        let node = transform_guard_expr(rhs, rule);
                        let ty = check_type(&node, rule);
                        let id = rule.register_def(lhs, ty);
                        guards.add_definition(id, node);
                    }
                    // comparison
                    s if is_relation(s) => {
                        let node = transform_guard_expr(process, rule);
                        _ = check_type(&node, rule);
                        guards.add_constraint(node);
                    }
                    // type constraint
                    s if is_type_name(s) => {
                        for arg in args {
                            match arg {
                                ASTNode::Link { name, .. } => {
                                    let link = rule.get_link_by_name_mut(name);
                                    link.type_ = Some(ProcessConstraint::from(s));
                                }
                                ASTNode::Context { .. } => unimplemented!(),
                                _ => unreachable!("illegal guard in rule {}", rule.name),
                            }
                        }
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

fn transform_guard_expr(expr: &ASTNode, rule: &mut Rule) -> GuardNode {
    match expr {
        ASTNode::Atom { name, args, .. } => {
            let ty = solve_name(name);
            match ty {
                super::util::AtomType::Int(i) => GuardNode::Int(i),
                super::util::AtomType::Float(f) => GuardNode::Float(f),
                super::util::AtomType::Operator(op) => {
                    let lhs = &args[0];
                    let lhs = transform_guard_expr(lhs, rule);
                    let rhs = &args[1];
                    let rhs = transform_guard_expr(rhs, rule);
                    GuardNode::Binary(op, Box::new(lhs), Box::new(rhs))
                }
                super::util::AtomType::Char(..) | super::util::AtomType::Plain => unreachable!(),
            }
        }
        ASTNode::Link { name, .. } => {
            let link = rule.get_link(name);
            GuardNode::Var(link)
        }
        ASTNode::Context { .. } => {
            unimplemented!("context will be implemented in the future")
        }
        _ => {
            unreachable!()
        }
    }
}

fn check_type(guard: &GuardNode, rule: &mut Rule) -> ProcessConstraint {
    match guard {
        GuardNode::Binary(op, lhs, rhs) => {
            // propagate type
            let op = op.into();
            // Must be relational operator or arithmetic operator
            match op {
                ProcessConstraint::Int | ProcessConstraint::Float => {
                    _ = check_type_or_assign(lhs, rule, op);
                    _ = check_type_or_assign(rhs, rule, op);
                }
                ProcessConstraint::Ground | ProcessConstraint::Unary => {
                    // Left and right must be both LINK
                    match (lhs.as_ref(), rhs.as_ref()) {
                        (GuardNode::Var(l), GuardNode::Var(r)) => {
                            // override previous defined type
                            // TODO: multiple conflicting guard
                            let l = rule.get_link_by_id_mut(l.get_id());
                            l.type_ = Some(op);
                            let r = rule.get_link_by_id_mut(r.get_id());
                            r.type_ = Some(op);
                        }
                        _ => panic!("Unexpected variable type"),
                    }
                }
                ProcessConstraint::Hyperlink
                | ProcessConstraint::Unique
                | ProcessConstraint::String => unreachable!(),
            };

            op
        }
        _ => {
            unreachable!()
        }
    }
}

fn check_type_or_assign(
    guard: &GuardNode,
    rule: &mut Rule,
    ty: ProcessConstraint,
) -> ProcessConstraint {
    match guard {
        GuardNode::Binary(op, lhs, rhs) => {
            // propagate type
            let op = op.into();
            _ = check_type_or_assign(lhs, rule, op);
            _ = check_type_or_assign(rhs, rule, op);
            op
        }
        GuardNode::Var(Process::Link(link)) => {
            let link = rule.get_link_by_id_mut(*link);

            if let Some(cur_ty) = link.type_ {
                // try override the type
                // number type could be implicitly converted to each other
                if cur_ty != ty {
                    if cur_ty == ProcessConstraint::Int && ty == ProcessConstraint::Float {
                        // int -> float
                        link.type_ = Some(ty);
                    } else if cur_ty == ProcessConstraint::Float && ty == ProcessConstraint::Int {
                        // float -> int
                    } else {
                        unreachable!()
                    }
                }
            } else {
                // set the type
                link.type_ = Some(ty);
            }
            ty
        }
        GuardNode::Int(_) => ProcessConstraint::Int,
        GuardNode::Float(_) => ProcessConstraint::Float,
        _ => {
            unreachable!()
        }
    }
}

fn is_relation(op: &str) -> bool {
    matches!(op, "!=" | "<" | ">" | "<=" | ">=")
}

fn is_type_name(name: &str) -> bool {
    matches!(name, "int" | "float" | "hlink" | "unary" | "ground")
}
