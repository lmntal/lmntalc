use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::util::Span;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TokenKind {
    // Literals
    Identifier(String),
    Int(i64),
    Float(f64),
    String(String),
    Operator(String),
    Keyword(String),

    // Reserved operators
    /// `@@`
    AtAt,
    /// `:-`
    ColonDash,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `|`
    Vert,
    /// `!`
    Bang,
    /// `$`
    Dollar,
    /// `=`
    Equal,

    // Brackets
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

/// Keywords in LMNtal.
pub const KEYWORD: [&str; 5] = ["int", "float", "ground", "unary", "uniq"];

impl TokenKind {
    fn to_string(&self) -> String {
        match self {
            TokenKind::Identifier(i) => i.clone(),
            TokenKind::Int(i) => i.to_string(),
            TokenKind::Float(f) => f.to_string(),
            TokenKind::String(s) => s.clone(),
            TokenKind::Operator(o) => o.clone(),
            TokenKind::Keyword(k) => k.clone(),
            TokenKind::AtAt => "@@".to_string(),
            TokenKind::ColonDash => ":-".to_string(),
            TokenKind::Comma => ",".to_string(),
            TokenKind::Dot => ".".to_string(),
            TokenKind::Vert => "|".to_string(),
            TokenKind::Bang => "!".to_string(),
            TokenKind::Dollar => "$".to_string(),
            TokenKind::Equal => "=".to_string(),
            TokenKind::LeftParen => "(".to_string(),
            TokenKind::RightParen => ")".to_string(),
            TokenKind::LeftBracket => "[".to_string(),
            TokenKind::RightBracket => "]".to_string(),
            TokenKind::LeftBrace => "{".to_string(),
            TokenKind::RightBrace => "}".to_string(),
        }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Identifier(i) => write!(f, "{}", i),
            TokenKind::Int(i) => write!(f, "{}", i),
            TokenKind::Float(fl) => write!(f, "{}", fl),
            TokenKind::String(s) => write!(f, "{}", s),
            TokenKind::Operator(o) => write!(f, "{}", o),
            TokenKind::Keyword(k) => write!(f, "{}", k),
            _ => write!(f, "{}", self.to_string()),
        }
    }
}

impl From<String> for TokenKind {
    fn from(other: String) -> TokenKind {
        TokenKind::Identifier(other)
    }
}

impl From<char> for TokenKind {
    fn from(other: char) -> TokenKind {
        match other {
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '|' => TokenKind::Vert,
            '!' => TokenKind::Bang,
            '$' => TokenKind::Dollar,
            '=' => TokenKind::Equal,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            _ => unreachable!(),
        }
    }
}

impl<'a> From<&'a str> for TokenKind {
    fn from(other: &'a str) -> TokenKind {
        TokenKind::Identifier(other.to_string())
    }
}

impl From<i64> for TokenKind {
    fn from(other: i64) -> TokenKind {
        TokenKind::Int(other)
    }
}

impl From<f64> for TokenKind {
    fn from(other: f64) -> TokenKind {
        TokenKind::Float(other)
    }
}

/// A valid token in LMNtal.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Token {
    /// The token's location relative to the rest of the files being
    /// processed.
    pub span: Span,
    /// What kind of token is this?
    pub kind: TokenKind,
}

impl Token {
    /// Create a new token out of a `Span` and something which can be turned
    /// into a `TokenKind`.
    pub fn new<K: Into<TokenKind>>(span: Span, kind: K) -> Token {
        let kind = kind.into();
        Token { span, kind }
    }
}

impl<T> From<T> for Token
where
    T: Into<TokenKind>,
{
    fn from(other: T) -> Token {
        Token::new(Span::dummy(), other)
    }
}
