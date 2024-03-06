/// This module contains the analyzer for LMNtal code.
pub mod analysis;
/// This module takes responsibility for the IR code generation.
pub mod codegen;
/// Includes the frontend for LMNtal code.
///
/// The frontend is responsible for parsing the input and transforming it into an AST.
pub mod frontend;
/// This module contains the IR for LMNtal code.
pub mod ir;
/// This module contains the data structures used to represent LMNtal code.
///
/// The AST will be transformed (see `tranform`) into these data structures, which are then
/// used to generate the output.
pub mod model;
/// This module contains the optimizer for LMNtal IR.
pub mod optimization;
/// This module contains the reporter for LMNtal code depending on `ariadne` crate.
pub mod report;
/// This module contains the backend for LMNtal code.
pub mod target;
/// Transforms the AST into a more usable form
pub mod transform;
/// Utilities for the compiler
pub mod util;

pub use frontend::ast::*;
pub use frontend::lexing::Lexer as LMNtalLexer;
pub use frontend::parsing::Parser as LMNtalParser;
pub use frontend::token::Token;
