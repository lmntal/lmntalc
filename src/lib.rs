/// This module contains the analyzer for LMNtal code.
pub mod analyzer;
/// This module contains the AST for LMNtal code.
pub mod ast;
/// This module contains the data structures used to represent LMNtal code.
///
/// The AST will be transformed (see `tranform`) into these data structures, which are then
/// used to generate the output.
pub mod data;
/// Lexer for LMNtal
pub mod lexing;
/// Parser for LMNtal
pub mod parsing;
/// Token definitions
pub mod token;
/// Transforms the AST into a more usable form
pub mod transform;
/// Utilities for the compiler
pub mod util;

/// This module contains the reporter for LMNtal code depending on `ariadne` crate.
///
/// To enable the reporter, use feature `ariadne`.
#[cfg(feature = "ariadne")]
pub mod report;
