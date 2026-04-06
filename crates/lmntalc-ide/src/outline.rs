use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutlineKind {
    InitialProcess,
    Rule,
    Membrane,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OutlineSymbol {
    pub name: String,
    pub kind: OutlineKind,
    pub span: Span,
    pub selection_span: Span,
    pub children: Vec<OutlineSymbol>,
}
