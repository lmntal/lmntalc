use crate::token::Operator;
use once_cell::sync::Lazy;
use std::{collections::HashMap, fmt::Display};

use super::id::AtomId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableId(usize);

impl From<usize> for VariableId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

impl From<VariableId> for usize {
    fn from(id: VariableId) -> Self {
        let VariableId(id) = id;
        id
    }
}

impl Display for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let VariableId(id) = self;
        write!(f, "{}", id)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Guard {
    /// List of conditions
    pub constraints: Vec<GuardNode>,
    /// Map from variable id to the expression
    pub definitions: HashMap<VariableId, Variable>,
    /// Map from variable name to the id
    pub defined: HashMap<String, VariableId>,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub ty: Option<ProcessConstraint>,
    pub node: GuardNode,
}

/// A function signature.
///
/// Currently only reserved `num` is supported.
/// User-defined functions will be supported in the future.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncSig {
    pub name: &'static str,
    pub args: Vec<ProcessConstraint>,
    pub ret: ProcessConstraint,
}

pub static RESERVED_FUNC: Lazy<Vec<FuncSig>> = Lazy::new(|| {
    vec![FuncSig {
        name: "num",
        args: vec![ProcessConstraint::Hyperlink],
        ret: ProcessConstraint::Int,
    }]
});

/// Source of a guard
#[derive(Debug, Clone)]
pub enum GuardSource {
    /// Refer to the atom at `AtomId`'s `usize`-th argument
    AtPortOfAtom(AtomId, usize),
    /// Refer to the atom that defined in guard
    Variable(VariableId),
    /// A placeholder for a variable that will be substituted later
    Placeholder(String),
}

#[derive(Debug, Clone)]
pub enum GuardNode {
    /// Variable, reference to a process (`Link` only for now)
    Var(GuardSource),
    /// Integer literal, like `1`
    Int(i64),
    /// Float literal, like `1.0`
    Float(f64),
    /// Process constraint, like `int(X)`, `float(X, Y)`, etc.
    Constraint(ProcessConstraint, Vec<GuardSource>),
    /// Function call, like `num(!X)`
    Function(FuncSig, Vec<GuardNode>),
    /// Binary operation, like `X + Y`, except assignment
    Binary(Operator, Box<GuardNode>, Box<GuardNode>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessConstraint {
    Int,
    Float,
    Hyperlink,
    Unique,
    Ground,
    Unary,
    String,
}

impl Guard {
    pub(crate) fn add_constraint(&mut self, node: GuardNode) {
        self.constraints.push(node);
    }

    pub(crate) fn add_definition(&mut self, id: VariableId, name: &str, node: GuardNode) {
        let var = Variable {
            name: name.to_string(),
            ty: None,
            node,
        };
        self.definitions.insert(id, var);
        self.defined.insert(name.to_string(), id);
    }

    pub(crate) fn defined(&self, name: &str) -> Option<VariableId> {
        self.defined.get(name).copied()
    }

    pub(crate) fn get(&self, id: VariableId) -> Option<&Variable> {
        self.definitions.get(&id)
    }
}

impl Display for ProcessConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ProcessConstraint::Int => "int",
            ProcessConstraint::Float => "float",
            ProcessConstraint::Unique => "uniq",
            ProcessConstraint::Ground => "ground",
            ProcessConstraint::Unary => "unary",
            ProcessConstraint::String => "string",
            ProcessConstraint::Hyperlink => "hlink",
        };
        write!(f, "{}", s)
    }
}

impl From<&str> for ProcessConstraint {
    fn from(value: &str) -> Self {
        match value {
            "int" => ProcessConstraint::Int,
            "float" => ProcessConstraint::Float,
            "uniq" => ProcessConstraint::Unique,
            "ground" => ProcessConstraint::Ground,
            "unary" => ProcessConstraint::Unary,
            "string" => ProcessConstraint::String,
            _ => unreachable!(),
        }
    }
}

impl From<Operator> for ProcessConstraint {
    fn from(value: Operator) -> Self {
        match value {
            Operator::IAdd
            | Operator::ISub
            | Operator::IMul
            | Operator::IDiv
            | Operator::IMod
            | Operator::IEq
            | Operator::INe
            | Operator::ILt
            | Operator::ILe
            | Operator::IGt
            | Operator::IGe => ProcessConstraint::Int,

            Operator::FAdd
            | Operator::FSub
            | Operator::FMul
            | Operator::FDiv
            | Operator::FEq
            | Operator::FNe
            | Operator::FLt
            | Operator::FLe
            | Operator::FGt
            | Operator::FGe => ProcessConstraint::Float,

            Operator::UnaryEq | Operator::UnaryNe => ProcessConstraint::Unary,
            Operator::GroundEq | Operator::GroundNe => ProcessConstraint::Ground,

            Operator::HyperlinkFuse | Operator::HyperlinkUnify => ProcessConstraint::Hyperlink,

            Operator::Equal => unreachable!(),
        }
    }
}
