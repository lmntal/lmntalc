use std::{collections::HashMap, fmt::Display};

use crate::token::Operator;

use super::{LinkId, Process};

#[derive(Debug, Default)]
pub struct Guard {
    /// List of conditions
    pub(crate) constraints: Vec<GuardNode>,
    /// Map from variable id to the expression
    pub(crate) definitions: HashMap<LinkId, GuardNode>,
}

impl Guard {
    pub(crate) fn add_constraint(&mut self, node: GuardNode) {
        self.constraints.push(node);
    }

    pub(crate) fn add_definition(&mut self, id: LinkId, node: GuardNode) {
        self.definitions.insert(id, node);
    }
}

#[derive(Debug)]
pub enum GuardNode {
    /// Variable, reference to a process (`Link` only for now)
    Var(Process),
    /// Integer literal, like `1`
    Int(i64),
    /// Float literal, like `1.0`
    Float(f64),
    /// Function constraint, like `int(X)`, `float(X, Y)`, etc.
    Func(ProcessConstraint, Vec<Process>),
    /// Assignment is apart from binary operation for the sake of generation
    Assign(Process, Box<GuardNode>),
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
