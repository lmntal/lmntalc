use lmntalc_ide::{
    Diagnostic as IdeDiagnostic, DiagnosticSeverity as IdeSeverity, DiagnosticStage as IdeStage,
    Pos, Source, Span,
};
use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Location, Position, Range, Uri,
};

pub fn to_lsp_diagnostics(
    source: &Source,
    uri: &Uri,
    diagnostics: &[IdeDiagnostic],
) -> Vec<Diagnostic> {
    diagnostics
        .iter()
        .map(|diagnostic| Diagnostic {
            range: diagnostic
                .primary_span
                .map(range_from_span)
                .unwrap_or_else(|| range_from_span(Span::default())),
            severity: Some(severity(diagnostic.severity)),
            source: Some(format!("lmntalc::{}", stage_name(diagnostic.stage))),
            message: format!("{}: {}", source.name(), diagnostic.message),
            related_information: if diagnostic.related_spans.is_empty() {
                None
            } else {
                Some(
                    diagnostic
                        .related_spans
                        .iter()
                        .map(|related| DiagnosticRelatedInformation {
                            location: Location {
                                uri: uri.clone(),
                                range: range_from_span(related.span),
                            },
                            message: related.message.clone(),
                        })
                        .collect(),
                )
            },
            ..Default::default()
        })
        .collect()
}

pub(crate) fn range_from_span(span: Span) -> Range {
    Range {
        start: position(span.low()),
        end: position(span.high()),
    }
}

fn position(pos: Pos) -> Position {
    Position {
        line: pos.line,
        character: pos.column,
    }
}

fn severity(severity: IdeSeverity) -> DiagnosticSeverity {
    match severity {
        IdeSeverity::Advice => DiagnosticSeverity::HINT,
        IdeSeverity::Warning => DiagnosticSeverity::WARNING,
        IdeSeverity::Error => DiagnosticSeverity::ERROR,
    }
}

fn stage_name(stage: IdeStage) -> &'static str {
    match stage {
        IdeStage::Lexing => "lexing",
        IdeStage::Parsing => "parsing",
        IdeStage::Semantics => "semantics",
        IdeStage::Lowering => "lowering",
        IdeStage::Backend => "backend",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lmntalc_ide::{DiagnosticStage, RelatedSpan};

    #[test]
    fn converts_diagnostics_with_stage_and_related_information() {
        let source = Source::from_string("a :-".to_string());
        let diagnostics = vec![IdeDiagnostic {
            stage: DiagnosticStage::Parsing,
            severity: IdeSeverity::Error,
            message: "Unexpected end of file".to_string(),
            primary_span: Some(Span::new(Pos::new(2, 0, 2), Pos::new(4, 0, 4))),
            related_spans: vec![RelatedSpan {
                span: Span::new(Pos::new(0, 0, 0), Pos::new(1, 0, 1)),
                message: "Rule starts here".to_string(),
            }],
        }];

        let converted = to_lsp_diagnostics(
            &source,
            &"file:///test.lmn".parse().expect("uri should parse"),
            &diagnostics,
        );
        assert_eq!(converted.len(), 1);
        assert_eq!(converted[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(converted[0].source.as_deref(), Some("lmntalc::parsing"));
        assert!(converted[0].related_information.is_some());
    }
}
