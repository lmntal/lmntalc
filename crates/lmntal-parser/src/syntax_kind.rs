#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    EOF,
    Error,
    Tombstone,
    Whitespace,
    Comment,

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
    Star,
    Slash,
    // '^'
    Caret,
    // '%'
    Percent,
    // '!'
    Ampersand,
    Underscore,
    Dot,
    Colon,
    Colon2,
    Eq,
    Eq2,
    Eq3,
    

    IntNumber,
    FloatNumber,

    IntKeyword,
    FloatKeyword,
    HyperlinkKeyword,
    GroundKeyword,
    UnaryKeyword,
    UniqueKeyword,

    NewLine,

    RULE,

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
    [$] => { SyntaxKind::Dollar };
}
