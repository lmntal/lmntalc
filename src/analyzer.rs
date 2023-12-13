pub mod process;

use crate::{ast::ASTNode, source_code::SourceCode};

pub trait Analyzer<'src> {
    type Error;
    type Warning;
    type Advice;

    fn new(source: &'src SourceCode) -> Self;
    fn source(&self) -> &'src SourceCode;
    fn analyze(&mut self, ast: &mut ASTNode);
    fn errors(&self) -> &[Self::Error];
    fn warnings(&self) -> &[Self::Warning];
    fn advices(&self) -> &[Self::Advice];
}
