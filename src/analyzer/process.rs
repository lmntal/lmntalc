use std::{collections::HashMap, fmt::Display, io, ops::Range};

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, Source};

use crate::{ast::ASTNode, source_code::SourceCode, util::Span};

use super::Analyzer;

#[derive(Debug)]
pub struct ProcessAnalyzer<'src> {
    source: &'src SourceCode,
    /// A map from Link name to the number of occurrences of the Link name.
    occurrence_counter: Vec<HashMap<String, usize>>,
    last_occurrence: Vec<HashMap<String, Span>>,
    /// errors for every process list
    errors: Vec<Vec<ProcessAnalyzerError>>,
}

#[derive(Debug)]
pub enum ProcessAnalyzerError {
    SingleOccurrenceOfLink { name: String, span: Span },
    MultipleOccurrenceOfLink { name: String, span: Span },
}

impl ProcessAnalyzerError {
    pub fn span(&self) -> Span {
        match self {
            Self::SingleOccurrenceOfLink { span, .. } => *span,
            Self::MultipleOccurrenceOfLink { span, .. } => *span,
        }
    }

    fn colored_string(&self) -> String {
        match self {
            Self::SingleOccurrenceOfLink { name, .. } => {
                format!("Link {} appears {}", name, "only once".fg(Color::Red))
            }
            Self::MultipleOccurrenceOfLink { name, .. } => {
                format!("Link {} appears more than {}", name, "twice".fg(Color::Red))
            }
        }
    }

    fn to_label<'a>(&'a self, source: &'a SourceCode, color: Color) -> Label<(&str, Range<usize>)> {
        let name = source.name();
        Label::new((name, self.span().into()))
            .with_message(self.colored_string())
            .with_color(color)
    }
}

impl Display for ProcessAnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SingleOccurrenceOfLink { name, .. } => {
                write!(f, "Link {} appears only once", name)
            }
            Self::MultipleOccurrenceOfLink { name, .. } => {
                write!(f, "Link {} appears more than twice", name)
            }
        }
    }
}

#[derive(Debug)]
pub enum ProcessAnalyzerWarning {}

impl ProcessAnalyzerWarning {
    pub fn span(&self) -> Span {
        todo!()
    }
}

impl<'src> ProcessAnalyzer<'src> {
    fn visit(&mut self, process: &ASTNode) {
        match process {
            ASTNode::ProcessList { processes, .. } => {
                // since we are visiting a new process list, we can clear the occurrence counter
                self.last_occurrence.push(HashMap::new());
                self.occurrence_counter.push(HashMap::new());
                let mut err = vec![];
                for process in processes {
                    self.visit(process);
                }
                let last_occurrence = self.last_occurrence.pop().unwrap();
                let occurrence_counter = self.occurrence_counter.pop().unwrap();
                for (name, count) in occurrence_counter {
                    if count > 2 {
                        err.push(ProcessAnalyzerError::MultipleOccurrenceOfLink {
                            name: name.clone(),
                            span: last_occurrence[&name],
                        });
                    }
                    if count == 1 {
                        err.push(ProcessAnalyzerError::SingleOccurrenceOfLink {
                            name: name.clone(),
                            span: last_occurrence[&name],
                        });
                    }
                }
                self.errors.push(err);
            }
            ASTNode::Membrane { process_lists, .. } => {
                for process_list in process_lists {
                    self.visit(process_list);
                }
            }
            ASTNode::Atom { args, .. } => {
                for arg in args {
                    self.visit(arg);
                }
            }
            ASTNode::Link {
                name,
                span,
                hyperlink,
            } => {
                if *hyperlink {
                    // ignore hyperlinks since they can appear from 1 to n times
                    return;
                }
                let last_occurrence = self.last_occurrence.last_mut().unwrap();
                let occurrence_counter = self.occurrence_counter.last_mut().unwrap();
                let count = occurrence_counter.entry(name.to_string()).or_insert(0);
                *count += 1;
                let last_occurrence = last_occurrence.entry(name.to_string()).or_insert(*span);
                *last_occurrence = *span;
            }
            ASTNode::Rule { .. } | ASTNode::Context { .. } => {}
        }
    }
}

impl<'src> Analyzer<'src> for ProcessAnalyzer<'src> {
    fn analyze(&mut self, process: &mut ASTNode) {
        self.visit(process);
    }

    fn report_errors(&self) -> io::Result<()> {
        for e in &self.errors {
            if e.is_empty() {
                continue;
            }
            let mut colors = ColorGenerator::new();
            let mut report = Report::build(
                ariadne::ReportKind::Error,
                self.source.name(),
                e.first().unwrap().span().low().as_usize(),
            )
            .with_message("In this list of processes:");
            for e in e {
                report = report.with_label(e.to_label(self.source, colors.next()));
            }
            report
                .finish()
                .eprint((self.source.name(), Source::from(self.source.source())))?;
        }
        Ok(())
    }

    fn report_warnings(&self) -> io::Result<()> {
        Ok(())
    }

    fn new(src: &'src crate::source_code::SourceCode) -> Self {
        Self {
            source: src,
            occurrence_counter: Vec::new(),
            last_occurrence: Vec::new(),
            errors: Vec::new(),
        }
    }
}
