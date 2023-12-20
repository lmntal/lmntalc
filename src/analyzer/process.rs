use std::collections::HashMap;

use crate::{ast::ASTNode, util::Span};

use super::{SemanticError, SemanticWarning};

#[derive(Debug)]
pub(super) struct ProcessAnalysisResult {
    pub(super) free_links: Vec<(String, Span)>,
    pub(super) errors: Vec<SemanticError>,
    pub(super) warnings: Vec<SemanticWarning>,
}

/// Do semantic analysis on the Membrane ASTNode
pub(super) fn analyze_membrane(ast: &ASTNode) -> ProcessAnalysisResult {
    let mut errors = Vec::new();
    let mut free_links = Vec::new();
    if let ASTNode::Membrane { process_lists, .. } = ast {
        for process_list in process_lists {
            let process_errors = analyze_process_list(process_list);
            errors.extend(process_errors.errors);
            free_links.extend(process_errors.free_links);
        }
    } else {
        unreachable!();
    }

    filter_link_occurances(free_links, errors)
}

pub(super) fn analyze_process_list(ast: &ASTNode) -> ProcessAnalysisResult {
    let mut errors = Vec::new();
    let mut free_links: Vec<(String, Span)> = Vec::new();
    if let ASTNode::ProcessList { processes, .. } = ast {
        for process in processes {
            match process {
                ASTNode::Membrane { .. } => {
                    let membrane_errors = analyze_membrane(process);
                    errors.extend(membrane_errors.errors);
                    free_links.extend(membrane_errors.free_links);
                }
                ASTNode::Atom { .. } => {
                    let atom_errors = analyze_atom(process);
                    errors.extend(atom_errors.errors);
                    free_links.extend(atom_errors.free_links);
                }
                ASTNode::Link {
                    name,
                    hyperlink,
                    span,
                } => {
                    // append ! to link name if it is a hyperlink
                    let name = if *hyperlink {
                        format!("{}!", name)
                    } else {
                        name.clone()
                    };
                    errors.push(SemanticError::TopLevelLinkOccurrence {
                        link: name,
                        span: *span,
                    });
                }
                _ => unreachable!(),
            }
        }
    } else {
        unreachable!()
    }

    filter_link_occurances(free_links, errors)
}

fn analyze_atom(ast: &ASTNode) -> ProcessAnalysisResult {
    let mut errors = Vec::new();
    let mut link_occurences = HashMap::new();
    if let ASTNode::Atom { args, .. } = ast {
        for arg in args {
            match arg {
                ASTNode::Link {
                    name,
                    hyperlink,
                    span,
                } => {
                    if !*hyperlink {
                        let occurs = link_occurences.entry(name.clone()).or_insert(Vec::new());
                        occurs.push(*span);
                    }
                }
                ASTNode::Atom { .. } => {
                    let atom_errors = analyze_atom(arg);
                    errors.extend(atom_errors.errors);
                }
                ASTNode::Membrane { .. } => {
                    let membrane_errors = analyze_membrane(arg);
                    errors.extend(membrane_errors.errors);
                    for free_link in membrane_errors.free_links {
                        let occurs = link_occurences
                            .entry(free_link.0.clone())
                            .or_insert(Vec::new());
                        occurs.push(free_link.1);
                    }
                }
                _ => unreachable!(),
            }
        }
    } else {
        unreachable!()
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
