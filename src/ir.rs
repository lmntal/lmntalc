use std::fmt::Display;

use crate::{
    data::{guard::ProcessConstraint, Data},
    token::Operator,
};

use owo_colors::OwoColorize;

/// Basic operations of LMNtal
#[derive(Debug, Clone)]
pub enum LMNtalIR {
    /// Create a new atom with specified name and arity
    CreateAtom {
        id: usize,
        name: String,
        arity: usize,
        data: Data,
    },
    /// Remove an atom with specified `id`
    ///
    /// Since it will be only used for in rule,
    /// the `id` will be the order of the pattern atom in the rule
    RemoveAtom {
        id: usize,
    },
    /// Remove an atom at specified `port` of atom with specified `id`
    RemoveAtomAt {
        id: usize,
        port: usize,
    },
    /// Clone an atom from specified `from` atom with specified `id`
    CloneAtom {
        id: usize,
        from_id: usize,
        from_port: usize,
    },

    /// Link atoms with specified ports
    Link {
        src: VarSource,
        dst: VarSource,
    },
    /// Link the atom `dst`'s port `dst_port` to the **atom linked to** `src`'s port `src_port`
    Relink {
        src: usize,
        src_port: usize,
        dst: VarSource,
    },

    /// Create a new hyperlink with specified name
    ///
    /// There is no explicit arity for hyperlink, since it is not fixed
    CreateHyperlink {
        id: usize,
        name: String,
    },
    /// Add an atom to a hyperlink
    LinkToHyperlink {
        atom: VarSource,
        hyperlink: VarSource,
    },
    /// Remove an atom from a hyperlink
    RemoveFromHyperlink {
        atom: VarSource,
        hyperlink: VarSource,
    },
    /// Fuse hyperlink `from` into hyperlink `into`, and remove `from`
    ///
    /// i.e. `into` will be the new hyperlink with union of atoms in `into` and `from`
    FuseHyperlink {
        into: VarSource,
        from: VarSource,
    },

    /// Find atoms with specified name and arity
    ///
    /// This will be translated into a range-based for loop since there may be multiple atoms meet the requirement
    FindAtom {
        id: usize,
        name: String,
        arity: usize,
    },
    /// Get the atom at specified `port` of atom with specified `id`
    GetAtomAtPort {
        id: usize,
        from: usize,
        port: usize,
        name: String,
        arity: usize,
    },
    /// Get the hyperlink at specified `port` of atom with specified `id`
    GetHyperlinkAtPort {
        id: usize,
        from: usize,
        port: usize,
    },
    /// Check if atoms (or hyperlinks) at id's port are equal
    AtomEqualityIdPort {
        /// List of atoms and their ports
        id_port_list: Vec<(usize, usize)>,
        /// Whether the atoms are equal or not
        eq: bool,
    },
    /// Check if atoms (or hyperlinks) are equal
    AtomEquality {
        /// List of atoms and their ports
        id_list: Vec<usize>,
        /// Whether the atoms are equal or not
        eq: bool,
        /// Whether they are hyperlinks or atoms
        hyperlinks: bool,
    },
    /// Check the type of an atom at atom `id`'s `port`
    CheckType {
        id: usize,
        port: usize,
        ty: ProcessConstraint,
    },
    /// Check the value using specified operation
    CheckValue(Operation),
    /// Define a temporary variable using specified operation with specified name and type
    DefineTempVar {
        id: usize,
        name: String,
        ty: ProcessConstraint,
        op: Operation,
    },
    Unify {
        into: VarSource,
        from: VarSource,
    },
}

impl Display for LMNtalIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LMNtalIR::CreateAtom {
                id,
                name,
                arity,
                data,
            } => {
                write!(
                    f,
                    "create atom at {} with name: {} arity: {} data: {}",
                    id.underline().bold(),
                    name,
                    arity,
                    data
                )
            }
            LMNtalIR::RemoveAtom { id } => write!(f, "remove atom at {}", id.underline().bold()),
            LMNtalIR::CloneAtom {
                id,
                from_id,
                from_port,
            } => {
                write!(
                    f,
                    "clone atom into {} from {} port {} ",
                    id.underline().bold(),
                    from_id,
                    from_port
                )
            }
            LMNtalIR::Link { src, dst } => write!(f, "link {} with {}", src, dst,),
            LMNtalIR::Relink { src, src_port, dst } => {
                write!(f, "relink {} port {} with {}", src, src_port, dst)
            }
            LMNtalIR::FindAtom { id, name, arity } => {
                write!(
                    f,
                    "find_atom into {} with name: {} arity: {}",
                    id.underline().bold(),
                    name,
                    arity
                )
            }
            LMNtalIR::GetAtomAtPort {
                id,
                from,
                port,
                name,
                arity,
            } => write!(
                f,
                "get atom into {} from {} port {} with name: {} arity: {}",
                id.underline().bold(),
                from.underline().bold(),
                port,
                name,
                arity
            ),
            LMNtalIR::GetHyperlinkAtPort { id, from, port } => write!(
                f,
                "get hyperlink into {} from {} port {}",
                id.underline().bold(),
                from.underline().bold(),
                port,
            ),
            LMNtalIR::AtomEqualityIdPort { id_port_list, eq } => {
                let predicate = if *eq { "are" } else { "are not" };
                write!(f, "atoms {} equal:\n\t\t", predicate)?;
                for (id, port) in id_port_list {
                    write!(f, "at {} port {},", id.underline().bold(), port)?;
                }
                Ok(())
            }
            LMNtalIR::AtomEquality {
                id_list,
                eq,
                hyperlinks,
            } => {
                let content = if *hyperlinks { "hyperlinks" } else { "atoms" };
                let predicate = if *eq { "are" } else { "are not" };
                write!(f, "{} {} equal:\n\t\t", content, predicate)?;
                for id in id_list {
                    write!(f, "at {},", id.underline().bold())?;
                }
                Ok(())
            }
            LMNtalIR::CheckType { id, port, ty } => write!(
                f,
                "check type at {} port {} is {}",
                id.underline().bold(),
                port,
                ty
            ),
            LMNtalIR::CheckValue(op) => write!(f, "check if {}", op),
            LMNtalIR::DefineTempVar { id, name, ty, op } => {
                write!(
                    f,
                    "define a temp var at {} {} {} {}",
                    id.underline().bold(),
                    name,
                    ty,
                    op
                )
            }
            LMNtalIR::RemoveAtomAt { id, port } => {
                write!(f, "remove atom at {} port {}", id.underline().bold(), port)
            }
            LMNtalIR::CreateHyperlink { id, name } => {
                write!(
                    f,
                    "create hyperlink at {} with name: {}",
                    id.underline().bold(),
                    name,
                )
            }
            LMNtalIR::LinkToHyperlink { atom, hyperlink } => {
                write!(f, "add atom {} to hyperlink {}", atom, hyperlink)
            }
            LMNtalIR::RemoveFromHyperlink { atom, hyperlink } => {
                write!(f, "remove atom {} from hyperlink {}", atom, hyperlink)
            }
            LMNtalIR::FuseHyperlink { into, from } => {
                write!(f, "fuse hyperlink {} into hyperlink {}", from, into)
            }
            LMNtalIR::Unify { into, from } => {
                write!(f, "unify {} with {}", from, into)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum VarSource {
    Head(usize, usize),
    Variable(usize),
    Body(usize, usize),
}

impl VarSource {
    pub fn id(&self) -> usize {
        match self {
            VarSource::Head(id, _) => *id,
            VarSource::Variable(id) => *id,
            VarSource::Body(id, _) => *id,
        }
    }
}

impl Display for VarSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarSource::Head(id, port) => write!(f, "head {} port {}", id.underline().bold(), port),
            VarSource::Variable(id) => write!(f, "temp {}", id.underline().bold()),
            VarSource::Body(id, port) => write!(f, "body {} port {}", id.underline().bold(), port),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Literal(Literal),
    Variable {
        source: VarSource,
        ty_: ProcessConstraint,
    },
    BinaryOP {
        op: BinaryOperator,
        lhs: Box<Operation>,
        rhs: Box<Operation>,
    },
    UnaryOP {
        op: UnaryOperator,
        operand: Box<Operation>,
    },
    FunctionCall {
        name: String,
        args: Vec<Operation>,
        ty_: ProcessConstraint,
    },
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Literal(l) => write!(f, "{}", l),
            Operation::Variable { source, ty_ } => match source {
                VarSource::Head(id, port) => {
                    write!(
                        f,
                        "(var at {} port {} with type {})",
                        id.underline().bold(),
                        port,
                        ty_
                    )
                }
                VarSource::Variable(id) => {
                    write!(f, "(var at {} with type {})", id.underline().bold(), ty_)
                }
                VarSource::Body(id, port) => {
                    write!(
                        f,
                        "(var at {} port {} with type {})",
                        id.underline().bold(),
                        port,
                        ty_
                    )
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

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
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

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
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

impl From<&Operator> for BinaryOperator {
    fn from(value: &Operator) -> Self {
        match value {
            Operator::IAdd | Operator::FAdd => Self::Add,
            Operator::ISub | Operator::FSub => Self::Sub,
            Operator::IMul | Operator::FMul => Self::Mul,
            Operator::IDiv | Operator::FDiv => Self::Div,
            Operator::IMod => Self::Mod,
            Operator::IEq | Operator::FEq | Operator::UnaryEq | Operator::GroundEq => Self::Eq,
            Operator::INe | Operator::FNe | Operator::UnaryNe | Operator::GroundNe => Self::Ne,
            Operator::ILt | Operator::FLt => Self::Lt,
            Operator::ILe | Operator::FLe => Self::Le,
            Operator::IGt | Operator::FGt => Self::Gt,
            Operator::IGe | Operator::FGe => Self::Ge,
            Operator::Equal => todo!(),
            Operator::HyperlinkFuse => todo!(),
            Operator::HyperlinkUnify => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Not,
    Neg,
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
