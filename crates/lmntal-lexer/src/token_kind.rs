pub enum TokenKind {
    Whitespace,
    Comment,
}

impl TokenKind {
    pub fn is_trivia(&self) -> bool {
        matches!(self, TokenKind::Whitespace | TokenKind::Comment)
    }
}
