pub mod process;

use crate::{util::Span, Membrane};

use self::process::analyze_membrane;

#[derive(Debug)]
pub enum SemanticError {
    MultipleLinkOccurrence { link: String, spans: Vec<Span> },
    FreeLinkOccurrence { link: String, span: Span },
    TopLevelLinkOccurrence { link: String, span: Span },
    MembraneInAtomArgument { span: Span },
}

#[derive(Debug)]
pub enum SemanticWarning {
    NoInitialProcess,
}

impl SemanticError {
    pub fn span(&self) -> Span {
        match self {
            SemanticError::MultipleLinkOccurrence { spans, .. } => spans
                .iter()
                .fold(*spans.first().unwrap(), |acc, span| acc.merge(*span)),
            SemanticError::FreeLinkOccurrence { span, .. } => *span,
            SemanticError::TopLevelLinkOccurrence { span, .. } => *span,
            SemanticError::MembraneInAtomArgument { span } => *span,
        }
    }
}

#[derive(Debug)]
pub struct SemanticAnalysisResult {
    pub warnings: Vec<SemanticWarning>,
    pub errors: Vec<SemanticError>,
}

/// Do semantic analysis on the Membrane ASTNode
pub fn analyze(root: &Membrane) -> SemanticAnalysisResult {
    if root.process_lists.is_empty() {
        return SemanticAnalysisResult {
            warnings: vec![SemanticWarning::NoInitialProcess],
            errors: vec![],
        };
    }

    let res = analyze_membrane(root);
    let mut errors = res.errors;

    for (link, span) in res.free_links {
        errors.push(SemanticError::FreeLinkOccurrence { link, span });
    }

    SemanticAnalysisResult {
        warnings: res.warnings,
        errors,
    }
}
