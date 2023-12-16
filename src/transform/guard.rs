use crate::{
    ast::{ASTNode, AtomName},
    data::{
        guard::{Guard, GuardNode, ProcessConstraint},
        rule::Rule,
        Process,
    },
    token::Operator,
};

pub(super) fn visit_guard(rule: &mut Rule, process_list: &ASTNode) -> Guard {
    let mut guards = Guard::default();
    if let ASTNode::ProcessList { processes, .. } = process_list {
        for process in processes {
            if let ASTNode::Atom { name, args, .. } = process {
                match name {
                    // assignment
                    AtomName::Operator(Operator::Equal) => {
                        assert!(args.len() == 2);
                        let lhs = match &args[0] {
                            ASTNode::Atom { name, .. } => name.to_string(),
                            _ => unreachable!(),
                        };
                        let rhs = &args[1];
                        let node = transform_guard_expr(rhs, rule);
                        let ty = check_type(&node, rule);
                        let id = rule.register_def(&lhs, ty);
                        guards.add_definition(id, node);
                    }
                    // comparison
                    AtomName::Operator(op) if op.is_relational() => {
                        let node = transform_guard_expr(process, rule);
                        _ = check_type(&node, rule);
                        guards.add_constraint(node);
                    }
                    // type constraint
                    AtomName::Keyword(s) => {
                        for arg in args {
                            match arg {
                                ASTNode::Link { name, .. } => {
                                    let link = rule.get_link_by_name_mut(name);
                                    link.type_ = Some(ProcessConstraint::from(s.as_str()));
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
        ASTNode::Atom { name, args, .. } => match name {
            AtomName::Int(i) => GuardNode::Int(*i),
            AtomName::Float(f) => GuardNode::Float(*f),
            AtomName::Operator(op) => {
                let lhs = &args[0];
                let lhs = transform_guard_expr(lhs, rule);
                let rhs = &args[1];
                let rhs = transform_guard_expr(rhs, rule);
                GuardNode::Binary(*op, Box::new(lhs), Box::new(rhs))
            }
            AtomName::Keyword(..) | AtomName::Char(..) | AtomName::Plain(..) => unreachable!(),
        },
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
            let op = (*op).into();
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
                            let l = rule.get_link_by_id_mut(l.get_id().into());
                            l.type_ = Some(op);
                            let r = rule.get_link_by_id_mut(r.get_id().into());
                            r.type_ = Some(op);
                        }
                        _ => panic!("Unexpected variable type"),
                    }
                }
                ProcessConstraint::Hyperlink
                | ProcessConstraint::Unique
                | ProcessConstraint::String => unimplemented!(),
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
            let op = (*op).into();
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
