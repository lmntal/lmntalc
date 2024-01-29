use std::{io, ops::Range};

use crate::{
    analysis::{SemanticAnalysisResult, SemanticError, SemanticWarning},
    frontend::lexing::{self, LexError, LexErrorType},
    frontend::parsing::{self, ParseError, ParseWarning, ParsingResult},
    util,
};
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, Source};

pub trait Reportable {
    fn labels<'a>(
        &'a self,
        source: &'a util::Source,
        colors: &mut ColorGenerator,
    ) -> Vec<Label<(&str, Range<usize>)>>;
    fn message(&self) -> String;
}

#[allow(unused_variables)]
pub trait Reporter<'src> {
    fn report_advices(&self, source: &'src util::Source) -> io::Result<()> {
        Ok(())
    }

    fn report_warnings(&self, source: &'src util::Source) -> io::Result<()> {
        Ok(())
    }

    fn report_errors(&self, source: &'src util::Source) -> io::Result<()> {
        Ok(())
    }

    fn report(&self, source: &'src util::Source) -> io::Result<()> {
        self.report_advices(source)?;
        self.report_warnings(source)?;
        self.report_errors(source)
    }
}

impl<'src> Reporter<'src> for SemanticAnalysisResult {
    fn report_errors(&self, source: &'src util::Source) -> io::Result<()> {
        for e in &self.errors {
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(
                ariadne::ReportKind::Error,
                source.name(),
                e.span().low().as_usize(),
            )
            .with_message(e.message());
            report = report.with_labels(e.labels(source, &mut colors));
            report
                .finish()
                .eprint((source.name(), Source::from(source.source())))?;
        }
        Ok(())
    }

    fn report_warnings(&self, source: &'src util::Source) -> io::Result<()> {
        for w in &self.warnings {
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(ariadne::ReportKind::Warning, source.name(), 0)
                .with_message(w.message());
            report = report.with_labels(w.labels(source, &mut colors));
            report
                .finish()
                .eprint((source.name(), Source::from(source.source())))?;
        }
        Ok(())
    }

    fn report(&self, source: &'src util::Source) -> io::Result<()> {
        self.report_warnings(source)?;
        self.report_errors(source)
    }
}

impl<'src> Reporter<'src> for ParsingResult {
    fn report_warnings(&self, source: &'src util::Source) -> io::Result<()> {
        for warn in &self.parsing_warnings {
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(
                ariadne::ReportKind::Warning,
                source.name(),
                warn.span.low().as_usize(),
            )
            .with_message(warn.message());
            report = report.with_labels(warn.labels(source, &mut colors));
            report
                .finish()
                .eprint((source.name(), Source::from(source.source())))?;
        }

        Ok(())
    }

    fn report_errors(&self, source: &'src util::Source) -> io::Result<()> {
        for e in &self.lexing_errors {
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(ariadne::ReportKind::Error, source.name(), e.offset)
                .with_message(e.message());
            report = report.with_labels(e.labels(source, &mut colors));
            if let Some(recover) = &e.recoverable {
                report = report.with_help(format!(
                    "Did you mean this: {}?",
                    recover.0.to_string().fg(Color::Blue)
                ));
            }
            report
                .finish()
                .eprint((source.name(), Source::from(source.source())))?;
        }

        for e in &self.parsing_errors {
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(
                ariadne::ReportKind::Error,
                source.name(),
                e.span.low().as_usize(),
            )
            .with_message(e.message());
            report = report.with_labels(e.labels(source, &mut colors));
            report
                .finish()
                .eprint((source.name(), Source::from(source.source())))?;
        }

        Ok(())
    }
}

impl Reportable for LexError {
    fn message(&self) -> String {
        match self.ty {
            lexing::LexErrorType::Expected(c) => {
                format!("Expected {}", c.to_string().fg(Color::Blue))
            }
            lexing::LexErrorType::UnexpectedCharacter(c) => {
                format!("Unexpected {}", c.to_string().fg(Color::Red))
            }
            lexing::LexErrorType::UncompleteNumber => "Uncomplete number".to_owned(),
            lexing::LexErrorType::UncompleteString => "Uncomplete string".to_owned(),
            lexing::LexErrorType::UnclosedQuote => "Unclosed quote".to_owned(),
            lexing::LexErrorType::UnclosedComment => "Unclosed comment".to_owned(),
            lexing::LexErrorType::UnmatchedBracket(c, _) => {
                format!("Unmatched bracket '{}'", c.to_string().fg(Color::Blue))
            }
        }
    }

    fn labels<'a>(
        &'a self,
        source: &'a util::Source,
        colors: &mut ColorGenerator,
    ) -> Vec<Label<(&str, Range<usize>)>> {
        let mut labels = vec![];
        if let LexErrorType::UnmatchedBracket(_, offset) = self.ty {
            let label = Label::new((source.name(), offset..offset + 1))
                .with_message("Did you forget to close the bracket here?".to_string())
                .with_color(colors.next());
            labels.push(label);
        }
        labels
    }
}

impl Reportable for ParseError {
    fn labels<'a>(
        &'a self,
        source: &'a util::Source,
        colors: &mut ColorGenerator,
    ) -> Vec<Label<(&str, Range<usize>)>> {
        vec![Label::new((source.name(), self.span.into()))
            .with_message(self.message())
            .with_color(colors.next())]
    }

    fn message(&self) -> String {
        match &self.ty {
            parsing::ParseErrorType::UnexpectedToken { expected, found } => {
                format!(
                    "Expected {}, but found {}",
                    expected.to_string().fg(Color::Blue),
                    found.to_string().fg(Color::Red)
                )
            }
            parsing::ParseErrorType::UnexpectedEOF => "Unexpected end of file".to_string(),
            parsing::ParseErrorType::WrongCase(ty) => {
                format!("The identifier should {}.", ty.should().fg(Color::Blue))
            }
        }
    }
}

impl Reportable for ParseWarning {
    fn labels<'a>(
        &'a self,
        source: &'a util::Source,
        colors: &mut ColorGenerator,
    ) -> Vec<Label<(&str, Range<usize>)>> {
        vec![Label::new((source.name(), self.span.into()))
            .with_message(self.message())
            .with_color(colors.next())]
    }

    fn message(&self) -> String {
        match &self.ty {
            parsing::ParseWarningType::MissingCommaBetweenProcesses => {
                "Missing comma between processes".to_string()
            }
        }
    }
}

impl Reportable for SemanticError {
    fn labels<'a>(
        &'a self,
        source: &'a util::Source,
        colors: &mut ColorGenerator,
    ) -> Vec<Label<(&str, Range<usize>)>> {
        let mut labels = vec![];
        match self {
            SemanticError::MultipleLinkOccurrence { spans, .. } => {
                let first = spans.first().unwrap();
                let label = Label::new((source.name(), (*first).into()))
                    .with_message("This is the first occurrence of this link".to_string())
                    .with_color(colors.next());
                labels.push(label);

                for span in spans.iter().skip(1) {
                    let label = Label::new((source.name(), (*span).into()))
                        .with_message("These are the other occurrences of this link".to_string())
                        .with_color(colors.next());
                    labels.push(label);
                }
            }
            SemanticError::FreeLinkOccurrence { span, .. } => {
                let label = Label::new((source.name(), (*span).into()))
                    .with_message("At here".to_string())
                    .with_color(colors.next());
                labels.push(label);
            }
            SemanticError::TopLevelLinkOccurrence { span, .. } => {
                let label = Label::new((source.name(), (*span).into()))
                    .with_message("At here".to_string())
                    .with_color(colors.next());
                labels.push(label)
            }
            SemanticError::MembraneInAtomArgument { span } => {
                let label = Label::new((source.name(), (*span).into()))
                    .with_message("At here".to_string())
                    .with_color(colors.next());
                labels.push(label)
            }
        }
        labels
    }

    fn message(&self) -> String {
        match self {
            SemanticError::MultipleLinkOccurrence { link, .. } => {
                format!("Multiple occurrence of link {}", link)
            }
            SemanticError::FreeLinkOccurrence { link, .. } => {
                format!("Free link {} is not allowed", link)
            }
            SemanticError::TopLevelLinkOccurrence { .. } => {
                "Top level link is not allowed".to_string()
            }
            SemanticError::MembraneInAtomArgument { .. } => {
                "Membrane is not allowed in atom argument".to_string()
            }
        }
    }
}

impl Reportable for SemanticWarning {
    #[allow(unused_variables)]
    fn labels<'a>(
        &'a self,
        source: &'a util::Source,
        colors: &mut ColorGenerator,
    ) -> Vec<Label<(&str, Range<usize>)>> {
        match self {
            SemanticWarning::NoInitialProcess => {
                vec![]
            }
        }
    }

    fn message(&self) -> String {
        match self {
            SemanticWarning::NoInitialProcess => "No initial process".to_string(),
        }
    }
}
