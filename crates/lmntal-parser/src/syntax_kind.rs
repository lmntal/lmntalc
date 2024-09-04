use std::{cell::LazyCell, sync::LazyLock};

use regex::Regex;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    Tombstone,
    EOF,

    LParen,
    RParen,
    LCurly,
    RCurly,
    LBrack,
    RBrack,
    LAngle,
    RAngle,

    At,
    Bang,
    Pound,
    Tilde,
    Question,
    Dollar,
    Plus,
    Minus,
    Comma,
    Star,
    Slash,
    Caret,
    Percent,
    Ampersand,
    Underscore,
    Dot,
    VerticalBar,
    Colon,
    SemiColon,
    Backslash,
    Eq,

    // Compound tokens
    At2,
    Eq2,
    Eq3,
    Colon2,
    ColonDash,
    Star2,
    AddDot,
    SubDot,
    MulDot,
    DivDot,
    RAngleEq,
    LAngleEq,
    RAngleDot,
    LAngleDot,
    RAngleEqDot,
    LAngleEqDot,
    EqColonEq,
    EqBackslashEq,
    EqColonEqDot,
    EqBackslashEqDot,
    BackSlashPlus,
    BackSlashEq,
    BackSlashEq2,
    RAngleLAngle,
    RAngleStarLAngle,
    RAnglePlusLAngle,
    LAngle2,
    RAngle2,
    RCurlyAt,
    RCurlySlash,
    RCurlySlashAt,
    RCurlyUnderscore,
    RCurlyUnderscoreAt,
    RCurlyUnderscoreSlash,
    RCurlyUnderscoreSlashAt,
    RCurlyStar,

    IntNumber,
    FloatNumber,
    Char,
    String,
    QuotedString,

    ModReserve,
    LogAndReserve,
    LogOrReserve,
    LogNotReserve,
    LogXorReserve,
    AshReserve,
    LshReserve,

    IntKeyword,
    FloatKeyword,
    HyperlinkKeyword,
    GroundKeyword,
    UnaryKeyword,
    UniqueKeyword,
    TypedefKeyword,

    AtomName,
    PathedAtomName,
    LinkName,

    Error,
    Comment,
    NewLine,
    Whitespace,

    ProcessList,
    Atom,
    Link,
    Hyperlink,
    Membrane,
    RuleHead,
    Guard,
    RuleBody,
    Rule,
    World,

    __LAST,
}

impl From<u16> for SyntaxKind {
    fn from(d: u16) -> Self {
        assert!(d <= (SyntaxKind::__LAST as u16));
        unsafe { std::mem::transmute::<u16, SyntaxKind>(d) }
    }
}

impl From<SyntaxKind> for u16 {
    fn from(d: SyntaxKind) -> Self {
        d as u16
    }
}

impl SyntaxKind {
    #[inline]
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::Whitespace | SyntaxKind::Comment)
    }

    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            SyntaxKind::IntKeyword
                | SyntaxKind::FloatKeyword
                | SyntaxKind::HyperlinkKeyword
                | SyntaxKind::GroundKeyword
                | SyntaxKind::UnaryKeyword
                | SyntaxKind::UniqueKeyword
                | SyntaxKind::TypedefKeyword
        )
    }

    pub fn is_punct(self) -> bool {
        matches!(
            self,
            SyntaxKind::LParen
                | SyntaxKind::RParen
                | SyntaxKind::LCurly
                | SyntaxKind::RCurly
                | SyntaxKind::LBrack
                | SyntaxKind::RBrack
                | SyntaxKind::LAngle
                | SyntaxKind::RAngle
                | SyntaxKind::At
                | SyntaxKind::Bang
                | SyntaxKind::Pound
                | SyntaxKind::Tilde
                | SyntaxKind::Question
                | SyntaxKind::Dollar
                | SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Comma
                | SyntaxKind::Star
                | SyntaxKind::Slash
                | SyntaxKind::Caret
                | SyntaxKind::Percent
                | SyntaxKind::Ampersand
                | SyntaxKind::Underscore
                | SyntaxKind::Dot
                | SyntaxKind::VerticalBar
                | SyntaxKind::Colon
                | SyntaxKind::SemiColon
                | SyntaxKind::Backslash
                | SyntaxKind::Eq
                | SyntaxKind::At2
                | SyntaxKind::Eq2
                | SyntaxKind::Eq3
                | SyntaxKind::Colon2
                | SyntaxKind::ColonDash
                | SyntaxKind::Star2
                | SyntaxKind::AddDot
                | SyntaxKind::SubDot
                | SyntaxKind::MulDot
                | SyntaxKind::DivDot
                | SyntaxKind::RAngleEq
                | SyntaxKind::LAngleEq
                | SyntaxKind::RAngleDot
                | SyntaxKind::LAngleDot
                | SyntaxKind::RAngleEqDot
                | SyntaxKind::LAngleEqDot
                | SyntaxKind::EqColonEq
                | SyntaxKind::EqBackslashEq
                | SyntaxKind::EqColonEqDot
                | SyntaxKind::EqBackslashEqDot
                | SyntaxKind::BackSlashPlus
                | SyntaxKind::BackSlashEq
                | SyntaxKind::BackSlashEq2
                | SyntaxKind::RAngleLAngle
                | SyntaxKind::RAngleStarLAngle
                | SyntaxKind::RAnglePlusLAngle
                | SyntaxKind::LAngle2
                | SyntaxKind::RAngle2
                | SyntaxKind::RCurlyAt
                | SyntaxKind::RCurlySlash
                | SyntaxKind::RCurlySlashAt
                | SyntaxKind::RCurlyUnderscore
                | SyntaxKind::RCurlyUnderscoreAt
                | SyntaxKind::RCurlyUnderscoreSlash
                | SyntaxKind::RCurlyUnderscoreSlashAt
                | SyntaxKind::RCurlyStar
        )
    }

    pub fn is_operator(self) -> bool {
        matches!(
            self,
            SyntaxKind::Eq
                | SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Star
                | SyntaxKind::Star2
                | SyntaxKind::Slash
                | SyntaxKind::AddDot
                | SyntaxKind::SubDot
                | SyntaxKind::MulDot
                | SyntaxKind::DivDot
                | SyntaxKind::ModReserve
                | SyntaxKind::LAngle
                | SyntaxKind::RAngle
                | SyntaxKind::LAngleEq
                | SyntaxKind::RAngleEq
                | SyntaxKind::LAngleDot
                | SyntaxKind::RAngleDot
                | SyntaxKind::LAngleEqDot
                | SyntaxKind::RAngleEqDot
                | SyntaxKind::EqColonEq
                | SyntaxKind::EqBackslashEq
                | SyntaxKind::EqColonEqDot
                | SyntaxKind::EqBackslashEqDot
                | SyntaxKind::Eq2
                | SyntaxKind::Eq3
                | SyntaxKind::BackSlashPlus
                | SyntaxKind::BackSlashEq
                | SyntaxKind::BackSlashEq2
                | SyntaxKind::RAngleLAngle
                | SyntaxKind::RAngleStarLAngle
                | SyntaxKind::RAnglePlusLAngle
                | SyntaxKind::RAngle2
                | SyntaxKind::LAngle2
                | SyntaxKind::LogAndReserve
                | SyntaxKind::LogOrReserve
                | SyntaxKind::LogNotReserve
                | SyntaxKind::LogXorReserve
                | SyntaxKind::AshReserve
                | SyntaxKind::LshReserve
        )
    }

    pub fn is_literal(self) -> bool {
        matches!(
            self,
            SyntaxKind::IntNumber
                | SyntaxKind::FloatNumber
                | SyntaxKind::Char
                | SyntaxKind::String
                | SyntaxKind::QuotedString
        )
    }

    pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
        let kw = match ident {
            "int" => SyntaxKind::IntKeyword,
            "float" => SyntaxKind::FloatKeyword,
            "hyperlink" => SyntaxKind::HyperlinkKeyword,
            "ground" => SyntaxKind::GroundKeyword,
            "unary" => SyntaxKind::UnaryKeyword,
            "unique" => SyntaxKind::UniqueKeyword,
            "typedef" => SyntaxKind::TypedefKeyword,
            _ => return None,
        };
        Some(kw)
    }

    pub fn from_reserved_operator(ident: &str) -> Option<SyntaxKind> {
        let op = match ident {
            "mod" => SyntaxKind::ModReserve,
            "logand" => SyntaxKind::LogAndReserve,
            "logor" => SyntaxKind::LogOrReserve,
            "lognot" => SyntaxKind::LogNotReserve,
            "logxor" => SyntaxKind::LogXorReserve,
            "ash" => SyntaxKind::AshReserve,
            "lsh" => SyntaxKind::LshReserve,
            _ => return None,
        };
        Some(op)
    }

    pub fn from_text(ident: &str) -> Option<SyntaxKind> {
        static ATOM_NAME: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^[a-z][a-zA-Z0-9_]*$").unwrap());
        static LINK_NAME: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^[A-Z_][A-Za-z_0-9]*$").unwrap());

        if ATOM_NAME.is_match(ident) {
            return Some(SyntaxKind::AtomName);
        } else if LINK_NAME.is_match(ident) {
            return Some(SyntaxKind::LinkName);
        }
        None
    }

    pub(crate) fn is_arithmetic_operator(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Plus
                | SyntaxKind::Minus
                | SyntaxKind::Star
                | SyntaxKind::Slash
                | SyntaxKind::ModReserve
                | SyntaxKind::AddDot
                | SyntaxKind::SubDot
                | SyntaxKind::MulDot
                | SyntaxKind::DivDot
        )
    }

    pub(crate) fn is_prefix_operator(&self) -> bool {
        matches!(
            self,
            SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::AddDot | SyntaxKind::SubDot
        )
    }

    pub(crate) fn prefix_binding_power(&self) -> u8 {
        if let SyntaxKind::Plus | SyntaxKind::Minus | SyntaxKind::AddDot | SyntaxKind::SubDot = self
        {
            9
        } else {
            unreachable!()
        }
    }

    pub(crate) fn infix_binding_power(&self) -> (u8, u8) {
        use SyntaxKind::*;
        match self {
            Star | Slash | ModReserve | MulDot | DivDot => (9, 10),
            Plus | Minus | AddDot | SubDot => (7, 8),
            LogXorReserve => (5, 6),
            LogAndReserve => (3, 4),
            LogOrReserve => (1, 2),
            _ => unreachable!(),
        }
    }
}

#[macro_export]
macro_rules! T {
    ['{'] => { SyntaxKind::LCurly };
    ['}'] => { SyntaxKind::RCurly };
    ['('] => { SyntaxKind::LParen };
    [')'] => { SyntaxKind::RParen };
    ['['] => { SyntaxKind::LBrack };
    [']'] => { SyntaxKind::RBrack };
    [<] => { SyntaxKind::LAngle };
    [>] => { SyntaxKind::RAngle };

    [@] => { SyntaxKind::At };
    [!] => { SyntaxKind::Bang };
    [#] => { SyntaxKind::Pound };
    [~] => { SyntaxKind::Tilde };
    [?] => { SyntaxKind::Question };
    [$] => { SyntaxKind::Dollar };
    [+] => { SyntaxKind::Plus };
    [-] => { SyntaxKind::Minus };
    [,] => { SyntaxKind::Comma };
    [*] => { SyntaxKind::Star };
    [/] => { SyntaxKind::Slash };
    [^] => { SyntaxKind::Caret };
    [%] => { SyntaxKind::Percent };
    [&] => { SyntaxKind::Ampersand };
    [_] => { SyntaxKind::Underscore };
    [.] => { SyntaxKind::Dot };
    [|] => { SyntaxKind::VerticalBar };
    [:] => { SyntaxKind::Colon };
    [;] => { SyntaxKind::SemiColon };
    ['\\'] => { SyntaxKind::Backslash };
    [=] => { SyntaxKind::Eq };

    ["@@"] => { SyntaxKind::At2 };
    [==] => { SyntaxKind::Eq2 };
    [===] => { SyntaxKind::Eq3 };
    [::] => { SyntaxKind::Colon2 };
    [:-] => { SyntaxKind::ColonDash };
    [**] => { SyntaxKind::Star2 };
    [+.] => { SyntaxKind::AddDot };
    [-.] => { SyntaxKind::SubDot };
    [*.] => { SyntaxKind::MulDot };
    [/.] => { SyntaxKind::DivDot };
    [>=] => { SyntaxKind::RAngleEq };
    [<=] => { SyntaxKind::LAngleEq };
    [>.] => { SyntaxKind::RAngleDot };
    [<.] => { SyntaxKind::LAngleDot };
    [>=.] => { SyntaxKind::RAngleEqDot };
    [<=.] => { SyntaxKind::LAngleEqDot };
    [=:=] => { SyntaxKind::EqColonEq };
    ["=\\="] => { SyntaxKind::EqBackslashEq };
    [=:=.] => { SyntaxKind::EqColonEqDot };
    ["=\\=."] => { SyntaxKind::EqBackslashEqDot };
    ["\\+"] => { SyntaxKind::BackSlashPlus };
    ["\\="] => { SyntaxKind::BackSlashEq };
    ["\\=="] => { SyntaxKind::BackSlashEq2 };
    [><] => { SyntaxKind::RAngleLAngle };
    [>*<] => { SyntaxKind::RAngleStarLAngle };
    [>+<] => { SyntaxKind::RAnglePlusLAngle };
    [<<] => { SyntaxKind::LAngle2 };
    [>>] => { SyntaxKind::RAngle2 };
    ["}@"] => { SyntaxKind::RCurlyAt };
    ["}/"] => { SyntaxKind::RCurlySlash };
    ["}/@"] => { SyntaxKind::RCurlySlashAt };
    ["}_"] => { SyntaxKind::RCurlyUnderscore };
    ["}_@"] => { SyntaxKind::RCurlyUnderscoreAt };
    ["}_/"] => { SyntaxKind::RCurlyUnderscoreSlash };
    ["}_/@"] => { SyntaxKind::RCurlyUnderscoreSlashAt };
    ["}*"] => { SyntaxKind::RCurlyStar };

    [mod] => { SyntaxKind::ModReserve };
    [and] => { SyntaxKind::LogAndReserve };
    [or] => { SyntaxKind::LogOrReserve };
    [not] => { SyntaxKind::LogNotReserve };
    [xor] => { SyntaxKind::LogXorReserve };
    [ash] => { SyntaxKind::AshReserve };
    [lsh] => { SyntaxKind::LshReserve };

    [int] => { SyntaxKind::IntKeyword };
    [float] => { SyntaxKind::FloatKeyword };
    [hyperlink] => { SyntaxKind::HyperlinkKeyword };
    [ground] => { SyntaxKind::GroundKeyword };
    [unary] => { SyntaxKind::UnaryKeyword };
    [unique] => { SyntaxKind::UniqueKeyword };
    [typedef] => { SyntaxKind::TypedefKeyword };
}
