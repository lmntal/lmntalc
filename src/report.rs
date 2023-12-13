use std::{io, ops::Range};

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, Source};
use lmntalc::{
    analyzer::{
        process::{ProcessAnalyzer, ProcessAnalyzerError},
        Analyzer,
    },
    source_code::SourceCode,
};

pub trait Reportable {
    fn label<'a>(&'a self, source: &'a SourceCode, color: Color) -> Label<(&str, Range<usize>)>;
    fn colored_string(&self) -> String;
}

pub trait Reporter {
    fn report_advices(&self) -> io::Result<()> {
        Ok(())
    }
    fn report_warnings(&self) -> io::Result<()>;
    fn report_errors(&self) -> io::Result<()>;
}

impl Reporter for ProcessAnalyzer<'_> {
    fn report_warnings(&self) -> io::Result<()> {
        Ok(())
    }

    fn report_errors(&self) -> io::Result<()> {
        for e in self.errors() {
            if e.is_empty() {
                continue;
            }
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(
                ariadne::ReportKind::Error,
                self.source().name(),
                e.first().unwrap().span().low().as_usize(),
            )
            .with_message("In this list of processes:");
            for e in e {
                report = report.with_label(e.label(self.source(), colors.next()));
            }
            report
                .finish()
                .eprint((self.source().name(), Source::from(self.source().source())))?;
        }
        Ok(())
    }
}

impl Reportable for ProcessAnalyzerError {
    fn label<'a>(&'a self, source: &'a SourceCode, color: Color) -> Label<(&str, Range<usize>)> {
        let name = source.name();
        Label::new((name, self.span().into()))
            .with_message(self.colored_string())
            .with_color(color)
    }

    fn colored_string(&self) -> String {
        match self {
            ProcessAnalyzerError::SingleOccurrenceOfLink { name, .. } => {
                format!("Link {} appears {}", name, "only once".fg(Color::Red))
            }
            ProcessAnalyzerError::MultipleOccurrenceOfLink { name, .. } => {
                format!("Link {} appears more than {}", name, "twice".fg(Color::Red))
            }
        }
    }
}
