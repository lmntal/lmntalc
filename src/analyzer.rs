pub mod process;

use std::io;

use crate::{ast::ASTNode, source_code::SourceCode};

pub trait Analyzer<'src> {
    fn new(src: &'src SourceCode) -> Self;
    fn analyze(&mut self, ast: &mut ASTNode);
    fn report_errors(&self) -> io::Result<()>;
    fn report_warnings(&self) -> io::Result<()>;
}

pub struct AnalyzerRunner<'src> {
    process_analyzer: process::ProcessAnalyzer<'src>,
}

impl<'src> AnalyzerRunner<'src> {
    pub fn new(src: &'src SourceCode) -> Self {
        Self {
            process_analyzer: process::ProcessAnalyzer::new(src),
        }
    }

    pub fn analyze(&mut self, ast: &mut ASTNode) {
        self.process_analyzer.analyze(ast);
    }

    pub fn report_errors(&self) -> io::Result<()> {
        self.process_analyzer.report_errors()
    }

    pub fn report_warnings(&self) -> io::Result<()> {
        self.process_analyzer.report_warnings()
    }
}
