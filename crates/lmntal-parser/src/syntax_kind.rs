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
    SlashEq,
    SlashEq2,
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

    Error,
    Comment,
    NewLine,
    Whitespace,

    ProcessList,
    Atom,
    Link,
    Membrane,
    RuleHead,
    Guard,
    RuleBody,
    Rule,

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
}

#[macro_export]
macro_rules! T {
    ['{'] => { SyntaxKind::LCurly };
    ['}'] => { SyntaxKind::RCurly };
    ['('] => { SyntaxKind::LParen };
    [')'] => { SyntaxKind::RParen };
    ['['] => { SyntaxKind::LBrack };
    [']'] => { SyntaxKind::RBrack };
    ['<'] => { SyntaxKind::LAngle };
    ['>'] => { SyntaxKind::RAngle };

    [@] => { SyntaxKind::At };
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
    ["\\"] => { SyntaxKind::Backslash };
    [=] => { SyntaxKind::Eq };

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
    [/=] => { SyntaxKind::SlashEq };
    [/==] => { SyntaxKind::SlashEq2 };
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
