use clap::ValueEnum;

use crate::codegen::IRSet;

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

pub trait Backend {
    fn new() -> Self;
    fn pretty_print(&mut self, generator: &IRSet) -> String;
    fn support_custom_lib(&self) -> bool {
        false
    }
    fn add_custom_lib(&mut self, _files: &Vec<std::path::PathBuf>) {
        let _ = _files;
    }
}
