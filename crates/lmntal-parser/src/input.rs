use crate::syntax_kind::SyntaxKind;

pub struct Input {
    pub kind: Vec<SyntaxKind>,
}

impl Input {
    #[inline]
    pub fn push(&mut self, kind: SyntaxKind) {
        self.kind.push(kind);
    }

    pub(crate) fn kind(&self, idx: usize) -> SyntaxKind {
        self.kind.get(idx).copied().unwrap_or(SyntaxKind::EOF)
    }
}
