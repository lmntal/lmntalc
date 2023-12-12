pub mod process;

use crate::ast::ASTNode;

pub trait Analyzer {
    type Error;
    type Warning;

    fn analyze(&mut self, ast: &mut ASTNode);
    fn errors(&self) -> &[Self::Error];
    fn warnings(&self) -> &[Self::Warning];
}
