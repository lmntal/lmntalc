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

    // Operators
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

    // Brackets
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    EOF,
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
            TokenKind::AtAt => write!(f, "@@"),
            TokenKind::ColonDash => write!(f, ":-"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Vert => write!(f, "|"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::Equal => write!(f, "="),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::EOF => write!(f, "<EOF>"),
            TokenKind::IAdd => write!(f, "+"),
            TokenKind::ISub => write!(f, "-"),
            TokenKind::IMul => write!(f, "*"),
            TokenKind::IDiv => write!(f, "/"),
            TokenKind::IMod => write!(f, "%"),
            TokenKind::FAdd => write!(f, "+."),
            TokenKind::FSub => write!(f, "-."),
            TokenKind::FMul => write!(f, "*."),
            TokenKind::FDiv => write!(f, "/."),
            TokenKind::IGt => write!(f, ">"),
            TokenKind::ILt => write!(f, "<"),
            TokenKind::IGe => write!(f, ">="),
            TokenKind::ILe => write!(f, "<="),
            TokenKind::IEq => write!(f, "=:=."),
            TokenKind::INe => write!(f, "=\\=."),
            TokenKind::FGt => write!(f, ">."),
            TokenKind::FLt => write!(f, "<."),
            TokenKind::FGe => write!(f, ">=."),
            TokenKind::FLe => write!(f, "<=."),
            TokenKind::FEq => write!(f, "=:=."),
            TokenKind::FNe => write!(f, "=\\=."),
            TokenKind::GroundEq => write!(f, "=="),
            TokenKind::GroundNe => write!(f, "\\="),
            TokenKind::UnaryEq => write!(f, "==="),
            TokenKind::UnaryNe => write!(f, "\\=="),
            TokenKind::HyperlinkFuse => write!(f, "><"),
            TokenKind::HyperlinkUnify => write!(f, "<<"),
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
            '+' => TokenKind::IAdd,
            '-' => TokenKind::ISub,
            '*' => TokenKind::IMul,
            '/' => TokenKind::IDiv,
            '%' => TokenKind::IMod,
            '>' => TokenKind::IGt,
            '<' => TokenKind::ILt,
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

impl TokenKind {
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
            Self::IGt
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
