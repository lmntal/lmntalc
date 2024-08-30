use crate::syntax_kind::SyntaxKind;

pub(crate) enum Event {
    Start {
        kind: SyntaxKind,
        forward_parent: Option<u32>,
    },
    Finish,

    Token {
        kind: SyntaxKind,
        n_raw_tokens: u8,
    },

    Error {
        msg: String,
    },
}

impl Event {
    pub(crate) fn tombstone() -> Self {
        Event::Start {
            kind: SyntaxKind::Tombstone,
            forward_parent: None,
        }
    }
}
