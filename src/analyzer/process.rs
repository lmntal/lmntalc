use std::collections::HashMap;

use crate::{ast::ASTNode, util::Span};

use super::Analyzer;

#[derive(Debug, Default)]
pub struct ProcessAnalyzer {
    /// A map from Link name to the number of occurrences of the Link name.
    occurrence_counter: HashMap<String, usize>,
    last_occurrence: HashMap<String, Span>,
    errors: Vec<ProcessAnalyzerError>,
}

#[derive(Debug)]
pub enum ProcessAnalyzerError {
    SingleOccurrenceOfLink { name: String, span: Span },
    MultipleOccurrenceOfLink { name: String, span: Span },
}

impl ProcessAnalyzer {
    fn visit(&mut self, process: &ASTNode) {
        match process {
            ASTNode::ProcessList { processes, .. } => {
                for process in processes {
                    self.visit(process);
                }
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
                let count = self.occurrence_counter.entry(name.to_string()).or_insert(0);
                *count += 1;
                let last_occurrence = self
                    .last_occurrence
                    .entry(name.to_string())
                    .or_insert(*span);
                *last_occurrence = *span;
                if *count > 2 {
                    self.errors
                        .push(ProcessAnalyzerError::MultipleOccurrenceOfLink {
                            name: name.clone(),
                            span: *span,
                        });
                }
            }
            ASTNode::Rule { .. } | ASTNode::Context { .. } => {}
        }
    }
}

impl Analyzer for ProcessAnalyzer {
    type Error = ProcessAnalyzerError;

    type Warning = ();

    fn analyze(&mut self, process: &mut ASTNode) {
        self.occurrence_counter.clear();
        self.errors.clear();
        self.visit(process);
        for (name, count) in &self.occurrence_counter {
            if *count == 1 {
                self.errors
                    .push(ProcessAnalyzerError::SingleOccurrenceOfLink {
                        name: name.clone(),
                        span: self.last_occurrence[name],
                    });
            }
        }
    }

    fn errors(&self) -> &[Self::Error] {
        &self.errors
    }

    fn warnings(&self) -> &[Self::Warning] {
        static EMPTY: &[()] = &[];
        EMPTY
    }
}
