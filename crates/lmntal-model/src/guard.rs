use std::fmt::Display;

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
            ProcessConstraint::Hyperlink => "hyperlink",
            ProcessConstraint::Unique => "unique",
            ProcessConstraint::Ground => "ground",
            ProcessConstraint::Unary => "unary",
            ProcessConstraint::String => "string",
        };
        write!(f, "{}", s)
    }
}