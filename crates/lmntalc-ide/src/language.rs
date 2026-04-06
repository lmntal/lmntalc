use std::collections::HashMap;

use lmntalc_core::syntax::{
    ast::{Atom, AtomName, FunctorName, Membrane, Process, ProcessList, Rule},
    token::KEYWORD,
};

use crate::{OutlineKind, OutlineSymbol, ReferenceIndex, SemanticKind, SemanticSpan, Span};

#[derive(Debug)]
pub(crate) struct LanguageInfo {
    outline: Vec<OutlineSymbol>,
    semantic_spans: Vec<SemanticSpan>,
    reference_index: ReferenceIndex,
}

impl LanguageInfo {
    pub(crate) fn outline(&self) -> &[OutlineSymbol] {
        &self.outline
    }

    pub(crate) fn semantic_spans(&self) -> &[SemanticSpan] {
        &self.semantic_spans
    }

    pub(crate) fn reference_index(&self) -> &ReferenceIndex {
        &self.reference_index
    }
}

#[derive(Debug, Default)]
struct ScopeResult {
    outline: Vec<OutlineSymbol>,
    link_occurrences: HashMap<String, Vec<Span>>,
    hyperlink_occurrences: HashMap<String, Vec<Span>>,
}

#[derive(Debug, Default)]
struct LanguageAnalyzer {
    semantic_spans: Vec<SemanticSpan>,
    queryable_spans: Vec<Span>,
    reference_groups: Vec<Vec<Span>>,
}

pub(crate) fn build_language_info(ast: &Membrane) -> LanguageInfo {
    let mut analyzer = LanguageAnalyzer::default();

    let mut init_result = ScopeResult::default();
    for process_list in &ast.process_lists {
        init_result.extend(analyzer.analyze_process_list(process_list, true));
    }
    analyzer.filter_links_top(init_result.link_occurrences);

    let mut rule_result = ScopeResult::default();
    for rule in &ast.rules {
        rule_result.extend(analyzer.analyze_rule(rule));
    }

    init_result
        .hyperlink_occurrences
        .extend(rule_result.hyperlink_occurrences);
    analyzer
        .reference_groups
        .extend(init_result.hyperlink_occurrences.into_values());

    let mut outline = Vec::new();
    if !ast.process_lists.is_empty() {
        let span = span_for_process_lists(&ast.process_lists).unwrap_or(ast.span);
        outline.push(OutlineSymbol {
            name: "init".to_string(),
            kind: OutlineKind::InitialProcess,
            span,
            selection_span: span,
            children: init_result.outline,
        });
    }
    outline.extend(rule_result.outline);

    LanguageInfo {
        outline,
        semantic_spans: analyzer.semantic_spans,
        reference_index: ReferenceIndex::new(analyzer.reference_groups, analyzer.queryable_spans),
    }
}

impl LanguageAnalyzer {
    fn analyze_process_list(&mut self, ast: &ProcessList, top_level: bool) -> ScopeResult {
        let mut result = ScopeResult::default();
        for process in &ast.processes {
            result.extend(self.analyze_process(process, top_level));
        }
        result
    }

    fn analyze_process(&mut self, process: &Process, top_level: bool) -> ScopeResult {
        let mut result = ScopeResult::default();
        match process {
            Process::Membrane(membrane) => result.extend(self.analyze_membrane(membrane)),
            Process::Atom(atom) => {
                for arg in &atom.args {
                    result.extend(self.analyze_process(arg, false));
                }
                self.add_semantic_span(atom.name.1, semantic_kind_for_atom(atom), true);
            }
            Process::Link(link) => {
                if !top_level {
                    self.add_semantic_span(link.span, SemanticKind::Link, true);
                    result
                        .link_occurrences
                        .entry(link.name.clone())
                        .or_default()
                        .push(link.span);
                }
            }
            Process::Hyperlink(hyperlink) => {
                self.add_semantic_span(hyperlink.span, SemanticKind::Hyperlink, true);
                result
                    .hyperlink_occurrences
                    .entry(hyperlink.name.0.clone())
                    .or_default()
                    .push(hyperlink.span);
            }
            Process::ProcessContext(context) => {
                self.add_semantic_span(context.span, SemanticKind::Context, true)
            }
            Process::RuleContext(context) => {
                self.add_semantic_span(context.span, SemanticKind::Context, true)
            }
            Process::LinkBundle(bundle) => {
                self.add_semantic_span(bundle.span, SemanticKind::Context, true)
            }
            Process::Rule(rule) => result.extend(self.analyze_rule(rule)),
        }
        result
    }

    fn analyze_membrane(&mut self, ast: &Membrane) -> ScopeResult {
        let mut result = ScopeResult::default();

        for process_list in &ast.process_lists {
            result.extend(self.analyze_process_list(process_list, false));
        }
        self.filter_links_inner(&mut result.link_occurrences);

        for rule in &ast.rules {
            result.extend(self.analyze_rule(rule));
        }

        let selection_span = selection_span(ast.name.1, ast.span);
        self.add_semantic_span(selection_span, SemanticKind::Membrane, true);

        let children = std::mem::take(&mut result.outline);
        result.outline.push(OutlineSymbol {
            name: if ast.name.0.is_empty() {
                "Anonymous membrane".to_string()
            } else {
                ast.name.0.clone()
            },
            kind: OutlineKind::Membrane,
            span: ast.span,
            selection_span,
            children,
        });
        result
    }

    fn analyze_rule(&mut self, ast: &Rule) -> ScopeResult {
        let selection_span = selection_span(ast.name.1, ast.span);
        self.add_semantic_span(selection_span, SemanticKind::Rule, false);

        let mut result = self.analyze_process_list(&ast.head, true);
        if let Some(propagation) = &ast.propagation {
            result.extend(self.analyze_process_list(propagation, true));
        }
        self.filter_links_inner(&mut result.link_occurrences);

        if let Some(body) = &ast.body {
            result.extend(self.analyze_process_list(body, true));
        }
        self.filter_links_top(result.link_occurrences.clone());
        result.link_occurrences.clear();

        result.outline.push(OutlineSymbol {
            name: ast.name.0.clone(),
            kind: OutlineKind::Rule,
            span: ast.span,
            selection_span,
            children: Vec::new(),
        });
        result
    }

    fn add_semantic_span(&mut self, span: Span, kind: SemanticKind, queryable: bool) {
        self.semantic_spans.push(SemanticSpan { span, kind });
        if queryable {
            self.queryable_spans.push(span);
        }
    }

    fn filter_links_top(&mut self, links: HashMap<String, Vec<Span>>) {
        for occurrences in links.into_values() {
            if occurrences.len() == 2 {
                self.reference_groups.push(occurrences);
            }
        }
    }

    fn filter_links_inner(&mut self, links: &mut HashMap<String, Vec<Span>>) {
        links.retain(|_, occurrences| match occurrences.len() {
            0 | 1 => true,
            2 => {
                self.reference_groups.push(occurrences.clone());
                false
            }
            _ => false,
        });
    }
}

impl ScopeResult {
    fn extend(&mut self, other: ScopeResult) {
        self.outline.extend(other.outline);
        for (link, occurrences) in other.link_occurrences {
            self.link_occurrences
                .entry(link)
                .or_default()
                .extend(occurrences);
        }
        for (hyperlink, occurrences) in other.hyperlink_occurrences {
            self.hyperlink_occurrences
                .entry(hyperlink)
                .or_default()
                .extend(occurrences);
        }
    }
}

fn semantic_kind_for_atom(atom: &Atom) -> SemanticKind {
    match &atom.name.0 {
        AtomName::Operator(_) => SemanticKind::OperatorAtom,
        AtomName::Int(_) | AtomName::Float(_) => SemanticKind::NumberAtom,
        AtomName::Char(_) => SemanticKind::StringAtom,
        AtomName::Functor(FunctorName::String(_) | FunctorName::QuotedString(_)) => {
            SemanticKind::StringAtom
        }
        AtomName::Functor(FunctorName::AtomName(name)) if KEYWORD.contains(&name.as_str()) => {
            SemanticKind::KeywordAtom
        }
        AtomName::Functor(_) => SemanticKind::Atom,
    }
}

fn span_for_process_lists(process_lists: &[ProcessList]) -> Option<Span> {
    process_lists
        .iter()
        .map(|process_list| process_list.span)
        .reduce(|lhs, rhs| lhs.merge(rhs))
}

fn selection_span(candidate: Span, fallback: Span) -> Span {
    if candidate.is_empty() {
        fallback
    } else {
        candidate
    }
}
