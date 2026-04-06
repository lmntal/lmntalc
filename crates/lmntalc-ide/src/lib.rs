use std::collections::HashMap;

use lmntalc_core::{
    codegen::{Emitter, IRSet},
    diagnostics::Diagnostic,
    lowering::{self, TransformResult},
    semantics::{analyze, SemanticAnalysisResult},
    syntax::{
        ast::{
            Atom, Hyperlink, Link, LinkBundle, Membrane, Process, ProcessContext, ProcessList,
            Rule, RuleContext,
        },
        lexing::{Lexer, LexingResult},
        parsing::{Parser, ParsingResult},
    },
    text::{Source, Span},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnalysisDepth {
    Semantic,
    Lowering,
    Ir,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AnalysisConfig {
    pub depth: AnalysisDepth,
}

impl Default for AnalysisConfig {
    fn default() -> Self {
        Self {
            depth: AnalysisDepth::Semantic,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DocumentSymbolKind {
    InitialProcess,
    Rule,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentSymbol {
    pub name: String,
    pub kind: DocumentSymbolKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxNodeKind {
    Membrane,
    Rule,
    ProcessList,
    Atom,
    Link,
    Hyperlink,
    ProcessContext,
    RuleContext,
    LinkBundle,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode {
    pub kind: SyntaxNodeKind,
    pub span: Span,
    pub name: Option<String>,
}

#[derive(Debug)]
pub struct DocumentSnapshot {
    uri: String,
    version: i32,
    source: Source,
    lexing: LexingResult,
    parsing: Option<ParsingResult>,
    semantics: Option<SemanticAnalysisResult>,
    lowering: Option<TransformResult>,
    ir: Option<IRSet>,
}

impl DocumentSnapshot {
    pub fn uri(&self) -> &str {
        &self.uri
    }

    pub fn version(&self) -> i32 {
        self.version
    }

    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn lexing(&self) -> &LexingResult {
        &self.lexing
    }

    pub fn parsing(&self) -> Option<&ParsingResult> {
        self.parsing.as_ref()
    }

    pub fn semantics(&self) -> Option<&SemanticAnalysisResult> {
        self.semantics.as_ref()
    }

    pub fn lowering(&self) -> Option<&TransformResult> {
        self.lowering.as_ref()
    }

    pub fn ir(&self) -> Option<&IRSet> {
        self.ir.as_ref()
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = self.lexing.diagnostics();
        if let Some(parsing) = &self.parsing {
            diagnostics.extend(parsing.diagnostics());
        }
        if let Some(semantics) = &self.semantics {
            diagnostics.extend(semantics.diagnostics());
        }
        if let Some(lowering) = &self.lowering {
            diagnostics.extend(lowering.diagnostics());
        }
        diagnostics
    }

    pub fn top_level_symbols(&self) -> Vec<DocumentSymbol> {
        let Some(parsing) = &self.parsing else {
            return Vec::new();
        };

        let mut symbols = Vec::new();
        if !parsing.root.process_lists.is_empty() {
            let span = parsing
                .root
                .process_lists
                .iter()
                .map(|process_list| process_list.span)
                .reduce(|lhs, rhs| lhs.merge(rhs))
                .unwrap_or(parsing.root.span);
            symbols.push(DocumentSymbol {
                name: "init".to_string(),
                kind: DocumentSymbolKind::InitialProcess,
                span,
            });
        }

        for rule in &parsing.root.rules {
            symbols.push(DocumentSymbol {
                name: rule.name.0.clone(),
                kind: DocumentSymbolKind::Rule,
                span: rule.span,
            });
        }

        symbols
    }

    pub fn node_at_offset(&self, offset: usize) -> Option<SyntaxNode> {
        self.find_best_node(|span| span_contains_offset(span, offset))
    }

    pub fn node_at_span(&self, span: Span) -> Option<SyntaxNode> {
        self.find_best_node(|candidate| candidate.contains(span))
    }

    fn find_best_node<F>(&self, predicate: F) -> Option<SyntaxNode>
    where
        F: Fn(Span) -> bool,
    {
        let parsing = self.parsing.as_ref()?;
        let mut nodes = Vec::new();
        collect_membrane_nodes(&parsing.root, &mut nodes);
        nodes
            .into_iter()
            .filter(|node| predicate(node.span))
            .min_by_key(|node| (node.span.len(), node_specificity(node.kind)))
    }
}

#[derive(Debug, Default)]
pub struct AnalysisSession {
    config: AnalysisConfig,
    documents: HashMap<String, DocumentSnapshot>,
}

impl AnalysisSession {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_config(config: AnalysisConfig) -> Self {
        Self {
            config,
            documents: HashMap::new(),
        }
    }

    pub fn set_document(&mut self, uri: impl Into<String>, version: i32, text: impl Into<String>) {
        let uri = uri.into();
        let snapshot = build_snapshot(self.config, uri.clone(), version, text.into());
        self.documents.insert(uri, snapshot);
    }

    pub fn remove_document(&mut self, uri: &str) -> Option<DocumentSnapshot> {
        self.documents.remove(uri)
    }

    pub fn snapshot(&self, uri: &str) -> Option<&DocumentSnapshot> {
        self.documents.get(uri)
    }

    pub fn diagnostics(&self, uri: &str) -> Vec<Diagnostic> {
        self.snapshot(uri)
            .map(DocumentSnapshot::diagnostics)
            .unwrap_or_default()
    }
}

fn build_snapshot(
    config: AnalysisConfig,
    uri: String,
    version: i32,
    text: String,
) -> DocumentSnapshot {
    let source = Source::new(uri.clone(), document_name(&uri), text);
    let lexing = Lexer::new(&source).lex();

    let parsing = if lexing.errors.is_empty() {
        Some(Parser::new().parse(lexing.tokens.clone()))
    } else {
        None
    };

    let semantics = parsing
        .as_ref()
        .filter(|parsing| parsing.parsing_errors.is_empty())
        .map(|parsing| analyze(&parsing.root));

    let lowering = if config.depth >= AnalysisDepth::Lowering {
        parsing
            .as_ref()
            .filter(|parsing| parsing.parsing_errors.is_empty())
            .zip(semantics.as_ref())
            .filter(|(_, semantics)| semantics.errors.is_empty())
            .map(|(parsing, _)| lowering::transform_lmntal(&parsing.root))
    } else {
        None
    };

    let ir = if config.depth >= AnalysisDepth::Ir {
        lowering
            .as_ref()
            .filter(|lowering| lowering.errors.is_empty())
            .map(|lowering| {
                let mut emitter = Emitter::new();
                emitter.generate(&lowering.program);
                emitter.finish()
            })
    } else {
        None
    };

    DocumentSnapshot {
        uri,
        version,
        source,
        lexing,
        parsing,
        semantics,
        lowering,
        ir,
    }
}

fn document_name(uri: &str) -> String {
    uri.rsplit('/').next().unwrap_or(uri).to_string()
}

fn span_contains_offset(span: Span, offset: usize) -> bool {
    let low = span.low().offset as usize;
    let high = span.high().offset as usize;
    if span.is_empty() {
        low == offset
    } else {
        low <= offset && offset < high
    }
}

fn collect_membrane_nodes(membrane: &Membrane, nodes: &mut Vec<SyntaxNode>) {
    nodes.push(SyntaxNode {
        kind: SyntaxNodeKind::Membrane,
        span: membrane.span,
        name: Some(membrane.name.0.clone()),
    });

    for process_list in &membrane.process_lists {
        collect_process_list_nodes(process_list, nodes);
    }

    for rule in &membrane.rules {
        collect_rule_nodes(rule, nodes);
    }
}

fn collect_rule_nodes(rule: &Rule, nodes: &mut Vec<SyntaxNode>) {
    nodes.push(SyntaxNode {
        kind: SyntaxNodeKind::Rule,
        span: rule.span,
        name: Some(rule.name.0.clone()),
    });
    collect_process_list_nodes(&rule.head, nodes);
    if let Some(propagation) = &rule.propagation {
        collect_process_list_nodes(propagation, nodes);
    }
    if let Some(guard) = &rule.guard {
        collect_process_list_nodes(guard, nodes);
    }
    if let Some(body) = &rule.body {
        collect_process_list_nodes(body, nodes);
    }
}

fn collect_process_list_nodes(process_list: &ProcessList, nodes: &mut Vec<SyntaxNode>) {
    nodes.push(SyntaxNode {
        kind: SyntaxNodeKind::ProcessList,
        span: process_list.span,
        name: None,
    });

    for process in &process_list.processes {
        collect_process_nodes(process, nodes);
    }
}

fn collect_process_nodes(process: &Process, nodes: &mut Vec<SyntaxNode>) {
    match process {
        Process::Atom(atom) => collect_atom_nodes(atom, nodes),
        Process::Membrane(membrane) => collect_membrane_nodes(membrane, nodes),
        Process::Link(link) => nodes.push(link_node(link)),
        Process::LinkBundle(bundle) => nodes.push(link_bundle_node(bundle)),
        Process::Hyperlink(hyperlink) => nodes.push(hyperlink_node(hyperlink)),
        Process::Rule(rule) => collect_rule_nodes(rule, nodes),
        Process::ProcessContext(context) => nodes.push(process_context_node(context)),
        Process::RuleContext(context) => nodes.push(rule_context_node(context)),
    }
}

fn collect_atom_nodes(atom: &Atom, nodes: &mut Vec<SyntaxNode>) {
    nodes.push(SyntaxNode {
        kind: SyntaxNodeKind::Atom,
        span: atom.span,
        name: Some(atom.name.0.to_string()),
    });
    for arg in &atom.args {
        collect_process_nodes(arg, nodes);
    }
}

fn link_node(link: &Link) -> SyntaxNode {
    SyntaxNode {
        kind: SyntaxNodeKind::Link,
        span: link.span,
        name: Some(link.name.clone()),
    }
}

fn link_bundle_node(bundle: &LinkBundle) -> SyntaxNode {
    SyntaxNode {
        kind: SyntaxNodeKind::LinkBundle,
        span: bundle.span,
        name: Some(bundle.name.0.clone()),
    }
}

fn hyperlink_node(hyperlink: &Hyperlink) -> SyntaxNode {
    SyntaxNode {
        kind: SyntaxNodeKind::Hyperlink,
        span: hyperlink.span,
        name: Some(hyperlink.name.0.clone()),
    }
}

fn process_context_node(context: &ProcessContext) -> SyntaxNode {
    SyntaxNode {
        kind: SyntaxNodeKind::ProcessContext,
        span: context.span,
        name: Some(context.name.0.clone()),
    }
}

fn rule_context_node(context: &RuleContext) -> SyntaxNode {
    SyntaxNode {
        kind: SyntaxNodeKind::RuleContext,
        span: context.span,
        name: Some(context.name.0.clone()),
    }
}

impl PartialOrd for AnalysisDepth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AnalysisDepth {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        rank(self).cmp(&rank(other))
    }
}

fn rank(depth: &AnalysisDepth) -> u8 {
    match depth {
        AnalysisDepth::Semantic => 0,
        AnalysisDepth::Lowering => 1,
        AnalysisDepth::Ir => 2,
    }
}

fn node_specificity(kind: SyntaxNodeKind) -> u8 {
    match kind {
        SyntaxNodeKind::Atom
        | SyntaxNodeKind::Link
        | SyntaxNodeKind::Hyperlink
        | SyntaxNodeKind::ProcessContext
        | SyntaxNodeKind::RuleContext
        | SyntaxNodeKind::LinkBundle => 0,
        SyntaxNodeKind::Rule | SyntaxNodeKind::Membrane => 1,
        SyntaxNodeKind::ProcessList => 2,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lmntalc_core::diagnostics::DiagnosticStage;

    #[test]
    fn supports_non_file_uris() {
        let mut session = AnalysisSession::new();
        session.set_document("untitled://scratch", 1, "a.");

        let snapshot = session
            .snapshot("untitled://scratch")
            .expect("snapshot should exist");
        assert_eq!(snapshot.source().uri(), "untitled://scratch");
        assert_eq!(snapshot.source().name(), "scratch");
    }

    #[test]
    fn open_update_remove_flow_refreshes_snapshots() {
        let mut session = AnalysisSession::new();
        session.set_document("file:///test.lmn", 1, "a :-");
        assert!(session
            .diagnostics("file:///test.lmn")
            .iter()
            .any(|diagnostic| diagnostic.stage == DiagnosticStage::Parsing));

        session.set_document("file:///test.lmn", 2, "a.");
        let snapshot = session
            .snapshot("file:///test.lmn")
            .expect("snapshot should exist");
        assert_eq!(snapshot.version(), 2);
        assert!(session
            .diagnostics("file:///test.lmn")
            .iter()
            .all(|diagnostic| diagnostic.stage != DiagnosticStage::Parsing));

        assert!(session.remove_document("file:///test.lmn").is_some());
        assert!(session.snapshot("file:///test.lmn").is_none());
    }

    #[test]
    fn default_depth_stops_before_lowering_and_ir() {
        let mut session = AnalysisSession::new();
        session.set_document("file:///depth.lmn", 1, "a.");

        let snapshot = session
            .snapshot("file:///depth.lmn")
            .expect("snapshot should exist");
        assert!(snapshot.semantics().is_some());
        assert!(snapshot.lowering().is_none());
        assert!(snapshot.ir().is_none());
    }

    #[test]
    fn can_opt_in_to_lowering_and_ir() {
        let mut session = AnalysisSession::with_config(AnalysisConfig {
            depth: AnalysisDepth::Ir,
        });
        session.set_document("file:///ir.lmn", 1, "name @@ a :- b. a.");

        let snapshot = session
            .snapshot("file:///ir.lmn")
            .expect("snapshot should exist");
        assert!(snapshot.lowering().is_some());
        assert!(snapshot.ir().is_some());
    }

    #[test]
    fn exposes_symbols_and_node_queries() {
        let mut session = AnalysisSession::new();
        let source = "name @@ a(X) :- b(X). a(1).";
        session.set_document("file:///symbols.lmn", 1, source);

        let snapshot = session
            .snapshot("file:///symbols.lmn")
            .expect("snapshot should exist");
        let symbols = snapshot.top_level_symbols();
        assert!(symbols.iter().any(|symbol| {
            symbol.kind == DocumentSymbolKind::InitialProcess && symbol.name == "init"
        }));
        assert!(symbols
            .iter()
            .any(|symbol| { symbol.kind == DocumentSymbolKind::Rule && symbol.name == "name" }));

        let atom_offset = source.find("b(X)").expect("offset should exist");
        let node = snapshot
            .node_at_offset(atom_offset)
            .expect("node should exist at offset");
        assert_eq!(node.kind, SyntaxNodeKind::Atom);
        assert_eq!(node.name.as_deref(), Some("b"));

        let rule_span = snapshot
            .top_level_symbols()
            .into_iter()
            .find(|symbol| symbol.kind == DocumentSymbolKind::Rule)
            .expect("rule symbol should exist")
            .span;
        let node = snapshot
            .node_at_span(rule_span)
            .expect("node should exist at span");
        assert_eq!(node.kind, SyntaxNodeKind::Rule);
    }
}
