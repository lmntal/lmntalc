use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::util::Span;

/// Tokens in LMNtal.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TokenKind {
    // Literals
    Identifier(String),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Keyword(String),

    // Reserved operators
    /// `@@`
    AtAt,
    /// `:-`
    ColonDash,
    /// `@`
    At,
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
    /// `\`
    Backslash,

    /// Operators
    Operator(Operator),

    // Brackets
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    EOF,
}

#[derive(Clone, Debug, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Operator {
    /// Equal (`=`)
    Equal,
    /// Integer addition (`+`)
    IAdd,
    /// Integer subtraction (`-`)
    ISub,
    /// Integer multiplication (`*`)
    IMul,
    /// Integer division (`/`)
    IDiv,
    /// Integer modulo (`%`)
    IMod,
    /// Float addition (`+.`)
    FAdd,
    /// Float subtraction (`-.`)
    FSub,
    /// Float multiplication (`*.`)
    FMul,
    /// Float division (`/.`)
    FDiv,

    /// Integer greater than (`>`)
    IGt,
    /// Integer less than (`<`)
    ILt,
    /// Integer greater than or equal to (`>=`)
    IGe,
    /// Integer less than or equal to (`<=`)
    ILe,
    /// Integer equal to (`=:=`)
    IEq,
    /// Integer not equal to (`=\=`)
    INe,
    /// Float greater than (`>.`)
    FGt,
    /// Float less than (`<.`)
    FLt,
    /// Float greater than or equal to (`>=.`)
    FGe,
    /// Float less than or equal to (`<=.`)
    FLe,
    /// Float equal to (`=:=.`)
    FEq,
    /// Float not equal to (`=\=.`)
    FNe,

    /// Ground equal to (`==`)
    GroundEq,
    /// Ground not equal to (`\=`)
    GroundNe,
    /// Unary equal to (`===`)
    UnaryEq,
    /// Unary not equal to (`\==`)
    UnaryNe,

    // Hyperlink operators
    /// Hyperlink fuse (`><` or '>*<' or '>+<')
    HyperlinkFuse,
    /// Hyperlink unfuse (`<<` or `>>`)
    HyperlinkUnify,
}

/// Keywords in LMNtal.
pub const KEYWORD: [&str; 6] = ["int", "float", "hlink", "ground", "unary", "uniq"];

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Identifier(i) => write!(f, "{}", i),
            TokenKind::Int(i) => write!(f, "{}", i),
            TokenKind::Float(fl) => write!(f, "{}", fl),
            TokenKind::Char(c) => write!(f, "'{}'", c),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::Operator(o) => write!(f, "{}", o),
            TokenKind::Keyword(k) => write!(f, "{}", k),
            TokenKind::At => write!(f, "@"),
            TokenKind::AtAt => write!(f, "@@"),
            TokenKind::ColonDash => write!(f, ":-"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Vert => write!(f, "|"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::Backslash => write!(f, "\\"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::EOF => write!(f, "<EOF>"),
        }
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
            '=' => TokenKind::Operator(Operator::Equal),
            '\\' => TokenKind::Backslash,
            '+' => TokenKind::Operator(Operator::IAdd),
            '-' => TokenKind::Operator(Operator::ISub),
            '*' => TokenKind::Operator(Operator::IMul),
            '/' => TokenKind::Operator(Operator::IDiv),
            '%' => TokenKind::Operator(Operator::IMod),
            '>' => TokenKind::Operator(Operator::IGt),
            '<' => TokenKind::Operator(Operator::ILt),
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

impl TokenKind {
    pub fn is_relational(&self) -> bool {
        matches!(self, Self::Operator(o) if o.is_relational())
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(self, Self::Operator(o) if o.is_arithmetic())
    }

    pub fn operator(&self) -> Option<Operator> {
        match self {
            Self::Operator(o) => Some(*o),
            _ => None,
        }
    }
}

impl From<String> for TokenKind {
    fn from(other: String) -> TokenKind {
        TokenKind::Identifier(other)
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

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(_), Self::Identifier(_)) => true,
            (Self::Int(_), Self::Int(_)) => true,
            (Self::Float(_), Self::Float(_)) => true,
            (Self::Char(_), Self::Char(_)) => true,
            (Self::String(_), Self::String(_)) => true,
            (Self::Operator(_), Self::Operator(_)) => true,
            (Self::Keyword(_), Self::Keyword(_)) => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Operator {
    /// Is this token an operator?
    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            Self::IAdd
                | Self::ISub
                | Self::IMul
                | Self::IDiv
                | Self::IMod
                | Self::FAdd
                | Self::FSub
                | Self::FMul
                | Self::FDiv
        )
    }

    pub fn is_relational(&self) -> bool {
        matches!(
            self,
            Self::Equal
                | Self::IGt
                | Self::ILt
                | Self::IGe
                | Self::ILe
                | Self::IEq
                | Self::INe
                | Self::FGt
                | Self::FLt
                | Self::FGe
                | Self::FLe
                | Self::FEq
                | Self::FNe
                | Self::GroundEq
                | Self::GroundNe
                | Self::UnaryEq
                | Self::UnaryNe
                | Self::HyperlinkFuse
                | Self::HyperlinkUnify
        )
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Equal => write!(f, "="),
            Operator::IAdd => write!(f, "+"),
            Operator::ISub => write!(f, "-"),
            Operator::IMul => write!(f, "*"),
            Operator::IDiv => write!(f, "/"),
            Operator::IMod => write!(f, "%"),
            Operator::FAdd => write!(f, "+."),
            Operator::FSub => write!(f, "-."),
            Operator::FMul => write!(f, "*."),
            Operator::FDiv => write!(f, "/."),
            Operator::IGt => write!(f, ">"),
            Operator::ILt => write!(f, "<"),
            Operator::IGe => write!(f, ">="),
            Operator::ILe => write!(f, "<="),
            Operator::IEq => write!(f, "=:="),
            Operator::INe => write!(f, "=\\="),
            Operator::FGt => write!(f, ">."),
            Operator::FLt => write!(f, "<."),
            Operator::FGe => write!(f, ">=."),
            Operator::FLe => write!(f, "<=."),
            Operator::FEq => write!(f, "=:=."),
            Operator::FNe => write!(f, "=\\=."),
            Operator::GroundEq => write!(f, "=="),
            Operator::GroundNe => write!(f, "\\="),
            Operator::UnaryEq => write!(f, "==="),
            Operator::UnaryNe => write!(f, "\\=="),
            Operator::HyperlinkFuse => write!(f, "><"),
            Operator::HyperlinkUnify => write!(f, "<<"),
        }
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

    pub const fn eof() -> Token {
        Token {
            span: Span::dummy(),
            kind: TokenKind::EOF,
        }
    }

    pub fn pretty_print(&self) -> String {
        format!("{}", self.kind)
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}>", self.kind)
    }
}
