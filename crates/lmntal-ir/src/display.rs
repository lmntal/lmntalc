use std::fmt::Display;

use crate::ir::{BinaryOperator, LMNtalIR, Literal, Operation, UnaryOperator, VarSource};

impl Display for LMNtalIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LMNtalIR::CreateAtom {
                id,
                name,
                arity,
                data,
            } => {
                write!(f, "CreateAtom {}, {}, {}, {}", id, name, arity, data)
            }
            LMNtalIR::RemoveAtom { id } => write!(f, "RemoveAtom {}", id),
            LMNtalIR::CloneAtom {
                id,
                from_id,
                from_port,
            } => {
                write!(f, "CloneAtom {}, {}, {}", id, from_id, from_port)
            }
            LMNtalIR::Link { src, dst } => write!(f, "Link {}, {}", src, dst,),
            LMNtalIR::Relink { src, src_port, dst } => {
                write!(f, "Relink {}, {}, {}", src, src_port, dst)
            }
            LMNtalIR::FindAtom { id, name, arity } => {
                write!(f, "FindAtom {}, {}, {}", id, name, arity)
            }
            LMNtalIR::GetAtomAtPort {
                id,
                from,
                port,
                name,
                arity,
            } => write!(
                f,
                "GetAtom {}, [{}, {}], {}, {}",
                id, from, port, name, arity
            ),
            LMNtalIR::GetHyperlinkAtPort { id, from, port } => {
                write!(f, "GetHyperlink {}, [{}, {}]", id, from, port,)
            }
            LMNtalIR::AtomEqualityIdPort { id_port_list, eq } => {
                let predicate = if *eq { "" } else { "Not" };
                write!(f, "Atoms{}Equal:\n\t\t", predicate)?;
                for (id, port) in id_port_list {
                    write!(f, "[{}, {}],", id, port)?;
                }
                Ok(())
            }
            LMNtalIR::AtomEquality {
                id_list,
                eq,
                hyperlinks,
            } => {
                let content = if *hyperlinks { "Hyperlinks" } else { "Atoms" };
                let predicate = if *eq { "" } else { "Not" };
                write!(f, "{}{}Equal:\n\t\t", content, predicate)?;
                for id in id_list {
                    write!(f, "[{}],", id)?;
                }
                Ok(())
            }
            LMNtalIR::CheckType { id, port, ty } => {
                write!(f, "CheckType [{}, {}], {}", id, port, ty)
            }
            LMNtalIR::CheckValue(op) => write!(f, "CheckIf {}", op),
            LMNtalIR::DefineTempVar { id, name, ty, op } => {
                write!(f, "DefineTemp {}, {}, {}, {}", id, name, ty, op)
            }
            LMNtalIR::RemoveAtomAt { id, port } => {
                write!(f, "RemoveAtom [{}, {}]", id, port)
            }
            LMNtalIR::CreateHyperlink { id, name } => {
                write!(f, "CreateHyperlink {}, {}", id, name,)
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                write!(f, "AddAtom {}, {}", atom, hyperlink)
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                write!(f, "RemoveAtom {}, {}", atom, hyperlink)
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                write!(f, "FuseHyperlink {}, {}", from, into)
            }
            LMNtalIR::Unify { into, from } => {
                write!(f, "UnifyHyperlink {}, {}", from, into)
            }
        }
    }
}

impl Display for VarSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarSource::Head(id, port) => write!(f, "Head[{}, {}]", id, port),
            VarSource::Variable(id) => write!(f, "Temp[{}]", id),
            VarSource::Body(id, port) => write!(f, "Body[{}, {}]", id, port),
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Literal(l) => write!(f, "{}", l),
            Operation::Variable { source, ty_ } => match source {
                VarSource::Head(id, port) => {
                    write!(f, "(var at {} port {} with type {})", id, port, ty_)
                }
                VarSource::Variable(id) => {
                    write!(f, "(var at {} with type {})", id, ty_)
                }
                VarSource::Body(id, port) => {
                    write!(f, "(var at {} port {} with type {})", id, port, ty_)
                }
            },
            Operation::BinaryOP { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Operation::UnaryOP { op, operand } => write!(f, "({} {})", op, operand),
            Operation::FunctionCall { name, args, .. } => {
                write!(
                    f,
                    "{}({})",
                    name,
                    args.iter()
                        .map(|a| format!("{}", a))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Char(c) => write!(f, "{}", c),
            Literal::String(s) => write!(f, "{}", s),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            UnaryOperator::Not => "!",
            UnaryOperator::Neg => "-",
        };
        write!(f, "{}", s)
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
            BinaryOperator::Pow => "**",
            BinaryOperator::Eq => "==",
            BinaryOperator::Ne => "!=",
            BinaryOperator::Lt => "<",
            BinaryOperator::Le => "<=",
            BinaryOperator::Gt => ">",
            BinaryOperator::Ge => ">=",
        };
        write!(f, "{}", s)
    }
}
