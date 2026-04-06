use std::collections::HashMap;

use crate::{
    model::{
        guard::{Guard, GuardNode, GuardSource, ProcessConstraint, RESERVED_FUNC, VariableId},
        rule::Rule,
    },
    syntax::{
        ast::{AtomName, FunctorName, Process, ProcessList},
        token::Operator,
    },
    text::Span,
};

use super::TransformError;

pub(super) fn visit_guard(
    rule: &mut Rule,
    process_list: &ProcessList,
    errors: &mut Vec<TransformError>,
) -> Guard {
    let mut guards = Guard::default();
    let mut defined = HashMap::new();
    let mut id: usize = 0;
    for process in &process_list.processes {
        let Process::Atom(atom) = process else {
            errors.push(TransformError::UnsupportedGuard {
                span: process.span(),
                message: "guard clauses must be atoms".to_string(),
            });
            continue;
        };

        match &atom.name.0 {
            AtomName::Operator(Operator::Equal) => {
                if atom.args.len() != 2 {
                    errors.push(TransformError::UnsupportedGuard {
                        span: atom.span,
                        message: "guard assignments must have exactly two arguments".to_string(),
                    });
                    continue;
                }
                let lhs = match &atom.args[0] {
                    Process::Link(link) => link.name.to_string(),
                    other => {
                        errors.push(TransformError::UnsupportedGuard {
                            span: other.span(),
                            message: "guard assignment left-hand side must be a link".to_string(),
                        });
                        continue;
                    }
                };
                let rhs = &atom.args[1];
                if let Some(node) = transform_guard_expr(rhs, &defined, errors) {
                    guards.add_definition(id.into(), &lhs, node);
                    defined.insert(lhs, id.into());
                    id += 1;
                }
            }
            AtomName::Operator(op) if is_supported_guard_operator(*op) => {
                if let Some(node) = transform_guard_expr(process, &defined, errors) {
                    guards.add_constraint(node);
                }
            }
            AtomName::Operator(op) => {
                errors.push(TransformError::UnsupportedGuard {
                    span: atom.span,
                    message: format!("unsupported guard operator {}", op),
                });
            }
            AtomName::Functor(FunctorName::AtomName(name)) => {
                match constraint_from_keyword(name, atom.span) {
                    Ok(ty) => {
                        let mut vars = Vec::new();
                        for arg in &atom.args {
                            if let Process::Link(link) = arg {
                                vars.push(GuardSource::Placeholder(link.name.clone()));
                            } else {
                                errors.push(TransformError::UnsupportedGuard {
                                    span: arg.span(),
                                    message: "type constraints currently only accept links"
                                        .to_string(),
                                });
                            }
                        }
                        guards.add_constraint(GuardNode::Constraint(ty, vars));
                    }
                    Err(error) => errors.push(error),
                }
            }
            _ => {
                errors.push(TransformError::UnsupportedGuard {
                    span: atom.span,
                    message: format!("illegal guard expression in rule {}", rule.name),
                });
            }
        }
    }

    guards
}

fn transform_guard_expr(
    expr: &Process,
    defined: &HashMap<String, VariableId>,
    errors: &mut Vec<TransformError>,
) -> Option<GuardNode> {
    match expr {
        Process::Atom(atom) => match &atom.name.0 {
            AtomName::Int(i) => Some(GuardNode::Int(*i)),
            AtomName::Float(f) => Some(GuardNode::Float(*f)),
            AtomName::Operator(op) => {
                if !is_supported_guard_operator(*op) {
                    errors.push(TransformError::UnsupportedGuard {
                        span: atom.span,
                        message: format!("unsupported guard operator {}", op),
                    });
                    return None;
                }
                if atom.args.len() != 2 {
                    errors.push(TransformError::UnsupportedGuard {
                        span: atom.span,
                        message: "guard operators must have exactly two arguments".to_string(),
                    });
                    return None;
                }
                let lhs = transform_guard_expr(&atom.args[0], defined, errors)?;
                let rhs = transform_guard_expr(&atom.args[1], defined, errors)?;
                Some(GuardNode::Binary(*op, Box::new(lhs), Box::new(rhs)))
            }
            AtomName::Functor(name) => {
                if let Some(func) = RESERVED_FUNC
                    .iter()
                    .find(|func| func.name == name.to_string())
                {
                    let vars = atom
                        .args
                        .iter()
                        .filter_map(|arg| transform_guard_expr(arg, defined, errors))
                        .collect::<Vec<_>>();
                    Some(GuardNode::Function(func.clone(), vars))
                } else {
                    errors.push(TransformError::UnknownGuardFunction {
                        span: atom.span,
                        name: name.to_string(),
                    });
                    None
                }
            }
            AtomName::Char(_) => {
                errors.push(TransformError::UnsupportedGuard {
                    span: atom.span,
                    message: "character literals are not supported in guards yet".to_string(),
                });
                None
            }
        },
        Process::Hyperlink(hyperlink) => {
            if let Some(id) = defined.get(&hyperlink.name.0) {
                Some(GuardNode::Var(GuardSource::Variable(*id)))
            } else {
                Some(GuardNode::Var(GuardSource::Placeholder(format!(
                    "!{}",
                    hyperlink.name.0
                ))))
            }
        }
        Process::Link(link) => {
            if let Some(id) = defined.get(&link.name) {
                Some(GuardNode::Var(GuardSource::Variable(*id)))
            } else {
                Some(GuardNode::Var(GuardSource::Placeholder(link.name.clone())))
            }
        }
        Process::ProcessContext(context) => {
            errors.push(TransformError::UnsupportedProcessContext { span: context.span });
            None
        }
        Process::RuleContext(context) => {
            errors.push(TransformError::UnsupportedRuleContext { span: context.span });
            None
        }
        other => {
            errors.push(TransformError::UnsupportedGuard {
                span: other.span(),
                message: format!("unsupported guard operand {}", other.process_name()),
            });
            None
        }
    }
}

fn is_supported_guard_operator(op: Operator) -> bool {
    matches!(
        op,
        Operator::IAdd
            | Operator::ISub
            | Operator::IMul
            | Operator::IDiv
            | Operator::IMod
            | Operator::FAdd
            | Operator::FSub
            | Operator::FMul
            | Operator::FDiv
            | Operator::IGt
            | Operator::ILt
            | Operator::IGe
            | Operator::ILe
            | Operator::IEq
            | Operator::INe
            | Operator::FGt
            | Operator::FLt
            | Operator::FGe
            | Operator::FLe
            | Operator::FEq
            | Operator::FNe
    )
}

fn constraint_from_keyword(s: &str, span: Span) -> Result<ProcessConstraint, TransformError> {
    let constraint = match s {
        "int" => ProcessConstraint::Int,
        "float" => ProcessConstraint::Float,
        "hlink" => ProcessConstraint::Hyperlink,
        "unary" => ProcessConstraint::Unary,
        "ground" => {
            return Err(TransformError::UnsupportedGuardConstraint {
                span,
                constraint: ProcessConstraint::Ground,
            });
        }
        "uniq" => {
            return Err(TransformError::UnsupportedGuardConstraint {
                span,
                constraint: ProcessConstraint::Unique,
            });
        }
        _ => {
            return Err(TransformError::UnsupportedGuard {
                span,
                message: format!("illegal type constraint {s}"),
            });
        }
    };
    Ok(constraint)
}
