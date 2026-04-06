use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticKind {
    Rule,
    Membrane,
    Atom,
    Link,
    Hyperlink,
    Context,
    KeywordAtom,
    OperatorAtom,
    StringAtom,
    NumberAtom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SemanticSpan {
    pub span: Span,
    pub kind: SemanticKind,
}
