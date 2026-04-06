use serde::{Deserialize, Serialize};

use crate::{
    lowering::{TransformError, TransformResult, TransformWarning},
    semantics::{SemanticAnalysisResult, SemanticError, SemanticWarning},
    syntax::{
        lexing::{LexError, LexErrorType, LexWarning, LexWarningType, LexingResult},
        parsing::{ParseError, ParseErrorType, ParseWarning, ParseWarningType, ParsingResult},
    },
    text::Span,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticStage {
    Lexing,
    Parsing,
    Semantics,
    Lowering,
    Backend,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    Advice,
    Warning,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RelatedSpan {
    pub span: Span,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnostic {
    pub stage: DiagnosticStage,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub primary_span: Option<Span>,
    pub related_spans: Vec<RelatedSpan>,
}

impl Diagnostic {
    pub fn error(
        stage: DiagnosticStage,
        message: impl Into<String>,
        primary_span: Option<Span>,
    ) -> Self {
        Self::new(stage, DiagnosticSeverity::Error, message, primary_span)
    }

    pub fn warning(
        stage: DiagnosticStage,
        message: impl Into<String>,
        primary_span: Option<Span>,
    ) -> Self {
        Self::new(stage, DiagnosticSeverity::Warning, message, primary_span)
    }

    pub fn advice(
        stage: DiagnosticStage,
        message: impl Into<String>,
        primary_span: Option<Span>,
    ) -> Self {
        Self::new(stage, DiagnosticSeverity::Advice, message, primary_span)
    }

    pub fn with_related_span(mut self, span: Span, message: impl Into<String>) -> Self {
        self.related_spans.push(RelatedSpan {
            span,
            message: message.into(),
        });
        self
    }

    fn new(
        stage: DiagnosticStage,
        severity: DiagnosticSeverity,
        message: impl Into<String>,
        primary_span: Option<Span>,
    ) -> Self {
        Self {
            stage,
            severity,
            message: message.into(),
            primary_span,
            related_spans: Vec::new(),
        }
    }
}

impl LexingResult {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        diagnostics.extend(self.warnings.iter().map(diagnostic_for_lex_warning));
        diagnostics.extend(self.errors.iter().map(diagnostic_for_lex_error));
        diagnostics
    }
}

impl ParsingResult {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        diagnostics.extend(
            self.parsing_warnings
                .iter()
                .map(diagnostic_for_parse_warning),
        );
        diagnostics.extend(self.parsing_errors.iter().map(diagnostic_for_parse_error));
        diagnostics
    }
}

impl SemanticAnalysisResult {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        diagnostics.extend(self.warnings.iter().map(diagnostic_for_semantic_warning));
        diagnostics.extend(self.errors.iter().map(diagnostic_for_semantic_error));
        diagnostics
    }
}

impl TransformResult {
    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        diagnostics.extend(self.warnings.iter().map(diagnostic_for_transform_warning));
        diagnostics.extend(self.errors.iter().map(diagnostic_for_transform_error));
        diagnostics
    }
}

fn diagnostic_for_lex_error(error: &LexError) -> Diagnostic {
    let mut diagnostic = Diagnostic::error(
        DiagnosticStage::Lexing,
        match error.ty {
            LexErrorType::Expected(c) => format!("Expected character '{c}'"),
            LexErrorType::UnexpectedCharacter(c) => format!("Unexpected character '{c}'"),
            LexErrorType::UncompleteNumber => "Uncomplete number".to_string(),
            LexErrorType::UncompleteString => "Uncomplete string".to_string(),
            LexErrorType::UnclosedQuote => "Unclosed quote".to_string(),
            LexErrorType::UnclosedComment => "Unclosed comment".to_string(),
            LexErrorType::UnmatchedBracket(c, _) => format!("Unmatched bracket '{c}'"),
        },
        Some(Span::new(error.pos, error.pos)),
    );

    match error.ty {
        LexErrorType::UnmatchedBracket(_, offset) => {
            diagnostic = diagnostic.with_related_span(
                Span::new(offset, offset),
                "Did you forget to close the bracket here?",
            );
        }
        _ => {
            diagnostic = diagnostic.with_related_span(Span::new(error.pos, error.pos), "At here");
        }
    }

    diagnostic
}

fn diagnostic_for_lex_warning(warning: &LexWarning) -> Diagnostic {
    let message = match warning.ty {
        LexWarningType::StringInsideCharLiteral => "String literal inside character literal",
    };
    Diagnostic::warning(
        DiagnosticStage::Lexing,
        message,
        Some(Span::new(warning.pos, warning.pos)),
    )
    .with_related_span(Span::new(warning.pos, warning.pos), "At here")
}

fn diagnostic_for_parse_error(error: &ParseError) -> Diagnostic {
    let message = match &error.ty {
        ParseErrorType::UnexpectedToken { expected, found } => {
            format!("Expected {expected}, but found {found}")
        }
        ParseErrorType::UnexpectedEOF => "Unexpected end of file".to_string(),
        ParseErrorType::ExpectAnItem => "Expect an item".to_string(),
    };

    Diagnostic::error(DiagnosticStage::Parsing, message.clone(), Some(error.span))
        .with_related_span(error.span, message)
}

fn diagnostic_for_parse_warning(warning: &ParseWarning) -> Diagnostic {
    let message = match warning.ty {
        ParseWarningType::MissingCommaBetweenProcesses => {
            "Missing comma between processes".to_string()
        }
        ParseWarningType::MissingPeriodAtTheEnd => {
            "Missing period at the end of the process".to_string()
        }
    };

    Diagnostic::warning(
        DiagnosticStage::Parsing,
        message.clone(),
        Some(warning.span),
    )
    .with_related_span(warning.span, message)
}

fn diagnostic_for_transform_error(error: &TransformError) -> Diagnostic {
    let message = match error {
        TransformError::TopLevelLink { .. } => "Top level link is not supported".to_string(),
        TransformError::UnsupportedProcessContext { .. } => {
            "Process contexts are not supported yet".to_string()
        }
        TransformError::UnsupportedRuleContext { .. } => {
            "Rule contexts are not supported yet".to_string()
        }
        TransformError::UnsupportedNestedRule { .. } => {
            "Rules inside membranes are not supported yet".to_string()
        }
        TransformError::UnsupportedProcessInAtom { process, .. } => {
            format!("{process} is not supported in atom arguments")
        }
        TransformError::UnsupportedGuard { message, .. } => message.clone(),
        TransformError::UnknownGuardFunction { name, .. } => {
            format!("Unknown guard function {name}")
        }
        TransformError::UnsupportedGuardConstraint { constraint, .. } => {
            format!("Constraint {constraint} is not supported by code generation yet")
        }
        TransformError::UnconstrainedLink { link, .. } => format!("Link {link} is unconstrained"),
        TransformError::LinkTooManyOccurrence { link, .. } => {
            format!("Link {link} occurs too many times")
        }
        TransformError::GuardTypeMismatch {
            expected, found, ..
        } => format!("Guard type mismatch: expected {expected}, found {found}"),
    };

    Diagnostic::error(
        DiagnosticStage::Lowering,
        message.clone(),
        Some(error.span()),
    )
    .with_related_span(error.span(), message)
}

fn diagnostic_for_transform_warning(warning: &TransformWarning) -> Diagnostic {
    let message = match warning {
        TransformWarning::UnusedVariable { name, .. } => {
            format!("Guard variable {name} is never used")
        }
    };

    Diagnostic::warning(
        DiagnosticStage::Lowering,
        message.clone(),
        Some(warning.span()),
    )
    .with_related_span(warning.span(), message)
}

fn diagnostic_for_semantic_error(error: &SemanticError) -> Diagnostic {
    match error {
        SemanticError::MultipleLinkOccurrence { link, spans } => {
            let mut diagnostic = Diagnostic::error(
                DiagnosticStage::Semantics,
                format!("Multiple occurrence of link {link}"),
                spans.first().copied(),
            );
            if let Some(first) = spans.first() {
                diagnostic = diagnostic
                    .with_related_span(*first, "This is the first occurrence of this link");
            }
            for span in spans.iter().skip(1) {
                diagnostic = diagnostic
                    .with_related_span(*span, "These are the other occurrences of this link");
            }
            diagnostic
        }
        SemanticError::FreeLinkOccurrence { link, span } => Diagnostic::error(
            DiagnosticStage::Semantics,
            format!("Free link {link} is not allowed"),
            Some(*span),
        )
        .with_related_span(*span, "At here"),
        SemanticError::TopLevelLinkOccurrence { span, .. } => Diagnostic::error(
            DiagnosticStage::Semantics,
            "Top level link is not allowed",
            Some(*span),
        )
        .with_related_span(*span, "At here"),
        SemanticError::MembraneInAtomArgument { span } => Diagnostic::error(
            DiagnosticStage::Semantics,
            "Membrane is not allowed in atom argument",
            Some(*span),
        )
        .with_related_span(*span, "At here"),
    }
}

fn diagnostic_for_semantic_warning(warning: &SemanticWarning) -> Diagnostic {
    match warning {
        SemanticWarning::NoInitialProcess => {
            Diagnostic::warning(DiagnosticStage::Semantics, "No initial process", None)
        }
    }
}
