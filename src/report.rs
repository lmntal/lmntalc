use std::{io, ops::Range};

use ariadne::{ColorGenerator, Label, Report, Source as AriadneSource};
use lmntalc_core::{
    diagnostics::{Diagnostic, DiagnosticSeverity},
    text::Source,
};

use crate::compiler::Compilation;

pub trait Reporter<'src> {
    fn report(&self, source: &'src Source) -> io::Result<()>;
}

impl<'src> Reporter<'src> for Compilation {
    fn report(&self, source: &'src Source) -> io::Result<()> {
        for diagnostic in self.diagnostics() {
            render_diagnostic(source, &diagnostic)?;
        }
        Ok(())
    }
}

fn render_diagnostic(source: &Source, diagnostic: &Diagnostic) -> io::Result<()> {
    let mut colors = ColorGenerator::new();
    let span = diagnostic.primary_span.map_or(0..0, Range::<usize>::from);
    let kind = match diagnostic.severity {
        DiagnosticSeverity::Advice => ariadne::ReportKind::Advice,
        DiagnosticSeverity::Warning => ariadne::ReportKind::Warning,
        DiagnosticSeverity::Error => ariadne::ReportKind::Error,
    };

    let mut report = Report::build(kind, (source.name(), span)).with_message(&diagnostic.message);
    let labels = diagnostic
        .related_spans
        .iter()
        .map(|related| {
            Label::new((source.name(), Range::<usize>::from(related.span)))
                .with_message(related.message.clone())
                .with_color(colors.next())
        })
        .collect::<Vec<_>>();
    report = report.with_labels(labels);
    report
        .finish()
        .eprint((source.name(), AriadneSource::from(source.source())))?;
    Ok(())
}
