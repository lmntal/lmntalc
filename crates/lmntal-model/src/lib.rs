use std::fmt::Display;

pub mod guard;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableId(usize);

#[derive(Debug, Default, Clone)]
pub enum Data {
    #[default]
    Empty,
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Variable(VariableId),
}

impl Data {
    pub fn is_empty(&self) -> bool {
        matches!(self, Data::Empty)
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Empty => write!(f, "empty"),
            Data::Int(i) => write!(f, "{}", i),
            Data::Float(fl) => write!(f, "{}", fl),
            Data::Char(c) => write!(f, "\'{}\'", c),
            Data::String(s) => write!(f, "\"{}\"", s),
            Data::Variable(id) => write!(f, "var{}", id.0),
        }
    }
}