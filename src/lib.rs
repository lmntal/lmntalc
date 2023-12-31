/// This module contains the analyzer for LMNtal code.
pub mod analyzer;
/// This module contains the AST for LMNtal code.
pub mod ast;
/// This module contains the backend for LMNtal code.
pub mod backend;
/// This module contains the data structures used to represent LMNtal code.
///
/// The AST will be transformed (see `tranform`) into these data structures, which are then
/// used to generate the output.
pub mod data;
/// This module contains the backend code generator.
pub mod generator;
/// This module contains the IR for LMNtal code.
pub mod ir;
/// Lexer for LMNtal
pub mod lexing;
/// This module contains the optimizer for LMNtal IR.
pub mod optimizer;
/// Parser for LMNtal
pub mod parsing;
/// This module contains the reporter for LMNtal code depending on `ariadne` crate.
pub mod report;
/// Token definitions
pub mod token;
/// Transforms the AST into a more usable form
pub mod transform;
/// Utilities for the compiler
pub mod util;
