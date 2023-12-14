pub mod process;

use crate::{ast::ASTNode, util::Span};

use self::process::analyze_membrane;

#[derive(Debug)]
pub enum SemanticError {
    MultipleLinkOccurrence { link: String, spans: Vec<Span> },
    FreeLinkOccurrence { link: String, span: Span },
    TopLevelLinkOccurrence { link: String, span: Span },
}

impl SemanticError {
    pub fn span(&self) -> Span {
        match self {
            SemanticError::MultipleLinkOccurrence { spans, .. } => spans
                .iter()
                .fold(*spans.first().unwrap(), |acc, span| acc.merge(*span)),
            SemanticError::FreeLinkOccurrence { span, .. } => *span,
            SemanticError::TopLevelLinkOccurrence { span, .. } => *span,
        }
    }
}

#[derive(Debug)]
pub struct SemanticAnalysisResult {
    pub errors: Vec<SemanticError>,
}

/// Do semantic analysis on the Membrane ASTNode
pub fn analyze(ast: &ASTNode) -> SemanticAnalysisResult {
    let mut errors = Vec::new();
    let res = analyze_membrane(ast);
    errors.extend(res.errors);

    for (link, span) in res.free_links {
        errors.push(SemanticError::FreeLinkOccurrence { link, span });
    }

    SemanticAnalysisResult { errors }
}
