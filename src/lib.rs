/// The high-level compilation pipeline and entrypoints.
pub mod compiler;
/// Debug and pretty-print helpers for CLI workflows.
pub mod debug;
/// This module contains the optimizer for LMNtal IR.
pub mod optimization;
/// This module contains the reporter for LMNtal code depending on `ariadne` crate.
pub mod report;
/// This module contains the backend for LMNtal code.
pub mod target;

pub use debug::tree_root;
pub use lmntalc_core::codegen;
pub use lmntalc_core::diagnostics;
pub use lmntalc_core::ir;
pub use lmntalc_core::lowering;
pub use lmntalc_core::model;
pub use lmntalc_core::semantics;
pub use lmntalc_core::syntax;
pub use lmntalc_core::text;

pub use lmntalc_core::lowering as transform;
pub use lmntalc_core::semantics as analysis;
pub use lmntalc_core::syntax as frontend;

pub mod util {
    pub use lmntalc_core::text::{Pos, Source, Span, Spanned};
}

pub use lmntalc_core::syntax::ast::*;
pub use lmntalc_core::syntax::lexing::Lexer as LMNtalLexer;
pub use lmntalc_core::syntax::parsing::Parser as LMNtalParser;
pub use lmntalc_core::syntax::token::Token;
