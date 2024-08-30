use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::util::Span;

/// Tokens in LMNtal.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum TokenKind {
    // Literals
    /// An atom name, with pattern `[a-z][A-Za-z_0-9]*`.
    AtomName(String),
    /// A link name, with pattern `[A-Z_][A-Za-z_0-9]*`.
    LinkName(String),
    /// An atom name that is pathed, with pattern `AtomName\.AtomName`.
    PathedAtomName(Vec<String>),
    /// A symbol name, like `'abc'` or `'a''b'`.
    SymbolName(String),

    /// A number, see `Number`.
    Number(Number),
    /// A character, like `#"a"`.
    Char(char),
    /// A string, like `"abc"`.
    String(String),
    /// A string that is quoted, like `[:abc:]`.
    Quoted(String),

    /// Line comment starting with `//`/`#`/`%`.
    LineComment(String),
    /// Block comment starting with `/*` and ending with `*/`.
    BlockComment(String),

    // Reserved operators
    /// A keyword that is used to define a type.
    Typedef,
    /// `@`
    At,
    /// `@@`
    AtAt,
    /// `:`
    Colon,
    /// `:-`
    ColonDash,
    /// `,`
    Comma,
    /// `.`
    Period,
    /// `|`
    Vert,
    /// `!`
    Bang,
    /// `$`
    Dollar,
    /// `\`
    Backslash,
    /// `~`
    Tilde,

    /// Operators
    Operator(Operator),

    // Brackets
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    RBraceSlash,
    RBraceAt,
    RBraceSlashAt,
    RBraceUnderbar,
    RBraceUnderbarSlash,
    RBraceUnderbarAt,
    RBraceUnderbarSlashAt,
    RBraceAsterisk,

    EOF,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Number {
    Binary(i64),
    Octal(i64),
    Decimal(i64),
    Hexadecimal(i64),
    Float(f64),
}

#[derive(Clone, Debug, Copy, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Operator {
    /// Equal (`=`)
    Equal,
    /// Negative (`\+`)
    Negative,
    /// Question mark (`?`)
    Question,
    /// Double colon (`::`)
    DoubleColon,

    /// Integer addition (`+`)
    IAdd,
    /// Integer subtraction (`-`)
    ISub,
    /// Integer multiplication (`*`)
    IMul,
    /// Integer division (`/`)
    IDiv,
    /// Integer power (`**`)
    IPow,
    /// Integer modulo (`mod`)
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

    /// Logical and (`logand`)
    LogicalAnd,
    /// Logical or (`logor`)
    LogicalOr,
    /// Logical not (`lognot`)
    LogicalNot,
    /// Logical xor (`logxor`)
    LogicalXor,

    /// Arithmetic shift (`ash`)
    ArithmeticShift,
    /// Logical shift (`lsh`)
    LogicalShift,

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
pub const CHAR_OPERATORS: [&str; 5] = ["mod", "logand", "logor", "lognot", "logxor"];

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Number(num) => write!(f, "{}", num),
            TokenKind::Char(c) => write!(f, "'{}'", c),
            TokenKind::String(s) => write!(f, "\"{}\"", s),
            TokenKind::Operator(o) => write!(f, "{}", o),
            TokenKind::At => write!(f, "@"),
            TokenKind::AtAt => write!(f, "@@"),
            TokenKind::ColonDash => write!(f, ":-"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Period => write!(f, "."),
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
            TokenKind::LineComment(c) => write!(f, "// {}", c),
            TokenKind::BlockComment(c) => write!(f, "/* {} */", c),
            TokenKind::AtomName(name) => write!(f, "{}", name),
            TokenKind::PathedAtomName(names) => write!(f, "{}", names.join(".")),
            TokenKind::SymbolName(name) => write!(f, "{}", name),
            TokenKind::LinkName(name) => write!(f, "{}", name),
            TokenKind::Quoted(name) => write!(f, "[:{}:]", name),
            TokenKind::Typedef => write!(f, "typedef"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::RBraceSlash => write!(f, "}}/"),
            TokenKind::RBraceAt => write!(f, "}}@"),
            TokenKind::RBraceSlashAt => write!(f, "}}/@"),
            TokenKind::RBraceUnderbar => write!(f, "}}_"),
            TokenKind::RBraceUnderbarSlash => write!(f, "}}_/"),
            TokenKind::RBraceUnderbarAt => write!(f, "}}_@"),
            TokenKind::RBraceUnderbarSlashAt => write!(f, "}}_/@"),
            TokenKind::RBraceAsterisk => write!(f, "}}*"),
        }
    }
}

impl From<char> for TokenKind {
    fn from(other: char) -> TokenKind {
        match other {
            ',' => TokenKind::Comma,
            '.' => TokenKind::Period,
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
            '~' => TokenKind::Tilde,
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

    pub fn new_number(num: i64, base: u8) -> Self {
        match base {
            2 => Self::Number(Number::Binary(num)),
            8 => Self::Number(Number::Octal(num)),
            10 => Self::Number(Number::Decimal(num)),
            16 => Self::Number(Number::Hexadecimal(num)),
            _ => unreachable!(),
        }
    }
}

impl From<String> for TokenKind {
    fn from(other: String) -> TokenKind {
        TokenKind::AtomName(other)
    }
}

impl From<f64> for TokenKind {
    fn from(other: f64) -> TokenKind {
        TokenKind::Number(Number::Float(other))
    }
}

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(n1), Self::Number(n2)) => n1 == n2,
            (Self::Char(_), Self::Char(_)) => true,
            (Self::String(_), Self::String(_)) => true,
            (Self::Operator(op1), Self::Operator(op2)) => op1 == op2,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Binary(i) => write!(f, "{:b}", i),
            Number::Octal(i) => write!(f, "{:o}", i),
            Number::Decimal(i) => write!(f, "{}", i),
            Number::Hexadecimal(i) => write!(f, "{:x}", i),
            Number::Float(fl) => write!(f, "{}", fl),
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (Self::Binary(_), Self::Binary(_))
                | (Self::Octal(_), Self::Octal(_))
                | (Self::Decimal(_), Self::Decimal(_))
                | (Self::Hexadecimal(_), Self::Hexadecimal(_))
                | (Self::Float(_), Self::Float(_))
        )
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

    pub fn is_prefix(&self) -> bool {
        use Operator::*;
        matches!(self, ISub | IAdd | FAdd | FSub)
    }

    pub fn is_infix(&self) -> bool {
        matches!(
            self,
            Self::Equal
                | Self::IAdd
                | Self::ISub
                | Self::IMul
                | Self::IDiv
                | Self::IMod
                | Self::FAdd
                | Self::FSub
                | Self::FMul
                | Self::FDiv
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
                | Self::LogicalAnd
                | Self::LogicalOr
                | Self::LogicalNot
                | Self::LogicalXor
        )
    }

    pub fn prefix_binding_power(&self) -> u8 {
        use Operator::*;
        match self {
            ISub | IAdd | FAdd | FSub => 9,
            _ => unreachable!(),
        }
    }

    pub fn infix_binding_power(&self) -> (u8, u8) {
        use Operator::*;
        match self {
            IMul | IDiv | IMod | FMul | FDiv => (3, 4),
            IAdd | ISub | FAdd | FSub => (1, 2),
            _ => unreachable!(),
        }
    }

    pub fn parse(s: &str) -> Option<Operator> {
        match s {
            "=" => Some(Operator::Equal),
            "+" => Some(Operator::IAdd),
            "-" => Some(Operator::ISub),
            "*" => Some(Operator::IMul),
            "/" => Some(Operator::IDiv),
            "%" => Some(Operator::IMod),
            "+." => Some(Operator::FAdd),
            "-." => Some(Operator::FSub),
            "*." => Some(Operator::FMul),
            "/." => Some(Operator::FDiv),
            ">" => Some(Operator::IGt),
            "<" => Some(Operator::ILt),
            ">=" => Some(Operator::IGe),
            "<=" => Some(Operator::ILe),
            "=:=" => Some(Operator::IEq),
            "=\\=" => Some(Operator::INe),
            ">." => Some(Operator::FGt),
            "<." => Some(Operator::FLt),
            ">=." => Some(Operator::FGe),
            "<=." => Some(Operator::FLe),
            "=:=." => Some(Operator::FEq),
            "=\\=." => Some(Operator::FNe),
            "==" => Some(Operator::GroundEq),
            "\\=" => Some(Operator::GroundNe),
            "===" => Some(Operator::UnaryEq),
            "\\==" => Some(Operator::UnaryNe),
            "><" => Some(Operator::HyperlinkFuse),
            "<<" => Some(Operator::HyperlinkUnify),
            "\\+" => Some(Operator::Negative),
            "\\?" => Some(Operator::Question),
            "::" => Some(Operator::DoubleColon),
            "logand" => Some(Operator::LogicalAnd),
            "logor" => Some(Operator::LogicalOr),
            "lognot" => Some(Operator::LogicalNot),
            "logxor" => Some(Operator::LogicalXor),
            "ash" => Some(Operator::ArithmeticShift),
            "lsh" => Some(Operator::LogicalShift),
            _ => None,
        }
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
            Operator::IPow => write!(f, "**"),
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
            Operator::Negative => write!(f, "\\+"),
            Operator::Question => write!(f, "\\?"),
            Operator::DoubleColon => write!(f, "::"),
            Operator::LogicalAnd => write!(f, "logand"),
            Operator::LogicalOr => write!(f, "logor"),
            Operator::LogicalNot => write!(f, "lognot"),
            Operator::LogicalXor => write!(f, "logxor"),
            Operator::ArithmeticShift => write!(f, "ash"),
            Operator::LogicalShift => write!(f, "lsh"),
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
