use crate::token_kind::TokenKind;

pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}