use std::collections::HashMap;

use crate::{util::Span, Atom, Membrane, Process, ProcessList};

use super::{SemanticError, SemanticWarning};

#[derive(Debug)]
pub(super) struct ProcessAnalysisResult {
    pub(super) free_links: Vec<(String, Span)>,
    pub(super) errors: Vec<SemanticError>,
    pub(super) warnings: Vec<SemanticWarning>,
}

/// Do semantic analysis on the Membrane ASTNode
pub(super) fn analyze_membrane(ast: &Membrane) -> ProcessAnalysisResult {
    let mut errors = Vec::new();
    let mut free_links = Vec::new();
    for process_list in &ast.process_lists {
        let process_errors = analyze_process_list(process_list);
        errors.extend(process_errors.errors);
        free_links.extend(process_errors.free_links);
    }

    filter_link_occurances(free_links, errors)
}

pub(super) fn analyze_process_list(process_list: &ProcessList) -> ProcessAnalysisResult {
    let mut errors = Vec::new();
    let mut free_links: Vec<(String, Span)> = Vec::new();
    for process in &process_list.processes {
        match process {
            Process::Membrane(mem) => {
                let membrane_errors = analyze_membrane(mem);
                errors.extend(membrane_errors.errors);
                free_links.extend(membrane_errors.free_links);
            }
            Process::Atom(atom) => {
                let atom_errors = analyze_atom(atom);
                errors.extend(atom_errors.errors);
                free_links.extend(atom_errors.free_links);
            }
            Process::Link(link) => {
                errors.push(SemanticError::TopLevelLinkOccurrence {
                    link: link.name.clone(),
                    span: link.span,
                });
            }
            _ => {}
        }
    }

    filter_link_occurances(free_links, errors)
}

fn analyze_atom(atom: &Atom) -> ProcessAnalysisResult {
    let mut errors = Vec::new();
    let mut link_occurences = HashMap::new();
    for arg in &atom.args {
        match arg {
            Process::Link(link) => {
                let occurs = link_occurences
                    .entry(link.name.clone())
                    .or_insert(Vec::new());
                occurs.push(link.span);
            }
            Process::Atom(atom) => {
                let atom_errors = analyze_atom(atom);
                errors.extend(atom_errors.errors);
                for free_link in atom_errors.free_links {
                    let occurs = link_occurences
                        .entry(free_link.0.clone())
                        .or_insert(Vec::new());
                    occurs.push(free_link.1);
                }
            }
            Process::Membrane(mem) => {
                errors.push(SemanticError::MembraneInAtomArgument { span: mem.span });
            }
            _ => unreachable!(),
        }
    }

    for (link, occurences) in &link_occurences {
        if occurences.len() > 2 {
            errors.push(SemanticError::MultipleLinkOccurrence {
                link: link.clone(),
                spans: occurences.clone(),
            });
        }
    }

    ProcessAnalysisResult {
        errors,
        warnings: vec![],
        free_links: link_occurences
            .iter()
            .filter(|pair| pair.1.len() == 1)
            .map(|(k, v)| (k.clone(), v[0]))
            .collect(),
    }
}

fn filter_link_occurances(
    free_links: Vec<(String, Span)>,
    mut errors: Vec<SemanticError>,
) -> ProcessAnalysisResult {
    let mut link_occurences = HashMap::new();

    for (link, occurences) in free_links {
        let occurs = link_occurences.entry(link).or_insert(Vec::new());
        occurs.push(occurences);
    }

    let mut free_links = Vec::new();

    for (link, occurences) in link_occurences {
        if occurences.len() > 2 {
            errors.push(SemanticError::MultipleLinkOccurrence {
                link,
                spans: occurences,
            });
        } else if occurences.len() == 1 {
            free_links.push((link, occurences[0]));
        }
    }

    ProcessAnalysisResult {
        warnings: Vec::new(),
        errors,
        free_links,
    }
}
