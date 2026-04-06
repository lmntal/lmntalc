use clap::ValueEnum;

use crate::{codegen::IRSet, model::guard::ProcessConstraint};

mod common;
pub mod cpp;
pub mod java;
pub mod python;

#[derive(Debug, Clone, Copy)]
pub enum Target {
    Cpp,
    Python,
    Java,
}

impl Target {
    pub fn extension(&self) -> &'static str {
        match self {
            Target::Cpp => "cpp",
            Target::Python => "py",
            Target::Java => "java",
        }
    }

    pub fn emit(self, ir_set: &IRSet) -> Result<String, BackendError> {
        match self {
            Target::Cpp => cpp::CppBackend::new().emit(ir_set),
            Target::Python => python::PythonBackend::new().emit(ir_set),
            Target::Java => java::JavaBackend::new().emit(ir_set),
        }
    }
}

impl ValueEnum for Target {
    fn value_variants<'a>() -> &'a [Self] {
        &[Target::Cpp, Target::Python, Target::Java]
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(match self {
            Target::Cpp => clap::builder::PossibleValue::new("cpp").alias("c++"),
            Target::Python => clap::builder::PossibleValue::new("python").alias("py"),
            Target::Java => clap::builder::PossibleValue::new("java"),
        })
    }
}

#[derive(Debug)]
pub enum BackendError {
    UnsupportedConstraint { constraint: ProcessConstraint },
    UnsupportedFunction { function: String },
    UnsupportedData { kind: &'static str },
    UnsupportedInstruction { instruction: &'static str },
    UnsupportedOperator { operator: &'static str },
    MissingVariableType { variable_id: usize },
}

impl std::fmt::Display for BackendError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BackendError::UnsupportedConstraint { constraint } => {
                write!(f, "backend does not support constraint {}", constraint)
            }
            BackendError::UnsupportedFunction { function } => {
                write!(f, "backend does not support function {}", function)
            }
            BackendError::UnsupportedData { kind } => {
                write!(f, "backend does not support {} data yet", kind)
            }
            BackendError::UnsupportedInstruction { instruction } => {
                write!(f, "backend does not support instruction {}", instruction)
            }
            BackendError::UnsupportedOperator { operator } => {
                write!(f, "backend does not support operator {}", operator)
            }
            BackendError::MissingVariableType { variable_id } => {
                write!(
                    f,
                    "missing inferred type for temporary variable {}",
                    variable_id
                )
            }
        }
    }
}

impl std::error::Error for BackendError {}

pub trait Backend {
    fn new() -> Self;
    fn emit(&mut self, generator: &IRSet) -> Result<String, BackendError>;
}
