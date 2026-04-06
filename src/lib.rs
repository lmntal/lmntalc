/// The high-level compilation pipeline and entrypoints.
pub mod compiler;
/// Debug and pretty-print helpers for CLI workflows.
pub mod debug;
/// This module contains the reporter for LMNtal code depending on `ariadne` crate.
pub mod report;
/// This module contains the backend for LMNtal code.
pub mod target;

pub use debug::tree_root;
pub use lmntalc_core::diagnostics;
pub use lmntalc_core::ir;
pub use lmntalc_core::optimization;
pub use lmntalc_core::syntax;
pub use lmntalc_core::text;
