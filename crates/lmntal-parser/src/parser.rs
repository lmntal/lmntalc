use crate::{
    event::Event, input::Input, syntax_kind::SyntaxKind, token_set::TokenSet, util::DropBomb, T,
};

pub struct Parser<'i> {
    input: &'i Input,
    pos: usize,
    events: Vec<Event>,
}

pub struct Marker {
    pos: u32,
    bomb: DropBomb,
}

impl<'i> Parser<'i> {
    pub fn new(input: &'i Input) -> Self {
        Parser {
            input,
            pos: 0,
            events: Vec::new(),
        }
    }

    pub fn finish(self) -> Vec<Event> {
        self.events
    }

    /// Returns the kind of the current token.
    /// If parser has already reached the end of input,
    /// the special `EOF` kind is returned.
    pub(crate) fn current(&self) -> SyntaxKind {
        self.nth(0)
    }

    /// Lookahead operation: returns the kind of the next nth
    /// token.
    pub(crate) fn nth(&self, n: usize) -> SyntaxKind {
        assert!(n <= 4);

        self.input.kind(self.pos + n)
    }

    /// Checks if the current token is `kind`.
    pub(crate) fn at(&self, kind: SyntaxKind) -> bool {
        self.nth_at(0, kind)
    }

    pub(crate) fn nth_at(&self, n: usize, kind: SyntaxKind) -> bool {
        match kind {
            T!["@@"] => self.at_composite2(n, T![@], T![@]),
            T![::] => self.at_composite2(n, T![:], T![:]),
            T![:-] => self.at_composite2(n, T![:], T![-]),
            T![**] => self.at_composite2(n, T![*], T![*]),
            T![+.] => self.at_composite2(n, T![+], T![.]),
            T![-.] => self.at_composite2(n, T![-], T![.]),
            T![*.] => self.at_composite2(n, T![*], T![.]),
            T![/.] => self.at_composite2(n, T![/], T![.]),
            T![>=] => self.at_composite2(n, T![>], T![=]),
            T![<=] => self.at_composite2(n, T![<], T![=]),
            T![>.] => self.at_composite2(n, T![>], T![.]),
            T![<.] => self.at_composite2(n, T![<], T![.]),
            T![==] => self.at_composite2(n, T![=], T![=]),
            T!["\\+"] => self.at_composite2(n, T!['\\'], T![+]),
            T!["\\="] => self.at_composite2(n, T!['\\'], T![=]),
            T![><] => self.at_composite2(n, T![>], T![<]),
            T![<<] => self.at_composite2(n, T![<], T![<]),
            T![>>] => self.at_composite2(n, T![>], T![>]),
            T!["}@"] => self.at_composite2(n, T!['}'], T![@]),
            T!["}/"] => self.at_composite2(n, T!['}'], T![/]),
            T!["}_"] => self.at_composite2(n, T!['}'], T![_]),
            T!["}*"] => self.at_composite2(n, T!['}'], T![*]),

            T![===] => self.at_composite3(n, T![=], T![=], T![=]),
            T![>=.] => self.at_composite3(n, T![>], T![=], T![.]),
            T![<=.] => self.at_composite3(n, T![<], T![=], T![.]),
            T![=:=] => self.at_composite3(n, T![=], T![:], T![=]),
            T!["=\\="] => self.at_composite3(n, T![=], T!['\\'], T![=]),
            T!["\\=="] => self.at_composite3(n, T!['\\'], T![=], T![=]),
            T![>*<] => self.at_composite3(n, T![>], T![*], T![<]),
            T![>+<] => self.at_composite3(n, T![>], T![+], T![<]),
            T!["}/@"] => self.at_composite3(n, T!['}'], T![/], T![@]),
            T!["}_@"] => self.at_composite3(n, T!['}'], T![_], T![@]),
            T!["}_/"] => self.at_composite3(n, T!['}'], T![_], T![/]),

            T![=:=.] => self.at_composite4(n, T![=], T![:], T![=], T![.]),
            T!["}_/@"] => self.at_composite4(n, T!['}'], T![_], T![/], T![@]),

            _ => self.input.kind(self.pos + n) == kind,
        }
    }

    /// Consume the next token if `kind` matches.
    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        if !self.at(kind) {
            return false;
        }
        let n_raw_tokens = match kind {
            T!["@@"]
            | T![::]
            | T![:-]
            | T![**]
            | T![+.]
            | T![-.]
            | T![*.]
            | T![/.]
            | T![>=]
            | T![<=]
            | T![>.]
            | T![<.]
            | T![==]
            | T!["\\+"]
            | T!["\\="]
            | T![><]
            | T![<<]
            | T![>>]
            | T!["}@"]
            | T!["}/"]
            | T!["}_"]
            | T!["}*"] => 2,

            T![===]
            | T![>=.]
            | T![<=.]
            | T![=:=]
            | T!["=\\="]
            | T!["\\=="]
            | T![>*<]
            | T![>+<]
            | T!["}/@"]
            | T!["}_@"]
            | T!["}_/"] => 3,

            T![=:=.] | T!["}_/@"] => 4,
            _ => 1,
        };
        self.do_bump(kind, n_raw_tokens);
        true
    }

    fn at_composite2(&self, n: usize, k1: SyntaxKind, k2: SyntaxKind) -> bool {
        self.input.kind(self.pos + n) == k1 && self.input.kind(self.pos + n + 1) == k2
    }

    fn at_composite3(&self, n: usize, k1: SyntaxKind, k2: SyntaxKind, k3: SyntaxKind) -> bool {
        self.input.kind(self.pos + n) == k1
            && self.input.kind(self.pos + n + 1) == k2
            && self.input.kind(self.pos + n + 2) == k3
    }

    fn at_composite4(
        &self,
        n: usize,
        k1: SyntaxKind,
        k2: SyntaxKind,
        k3: SyntaxKind,
        k4: SyntaxKind,
    ) -> bool {
        self.input.kind(self.pos + n) == k1
            && self.input.kind(self.pos + n + 1) == k2
            && self.input.kind(self.pos + n + 2) == k3
            && self.input.kind(self.pos + n + 3) == k4
    }

    /// Checks if the current token is in `kinds`.
    pub(crate) fn at_ts(&self, kinds: TokenSet) -> bool {
        kinds.contains(self.current())
    }

    /// Starts a new node in the syntax tree. All nodes and tokens
    /// consumed between the `start` and the corresponding `Marker::complete`
    /// belong to the same node.
    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.push_event(Event::tombstone());
        Marker::new(pos)
    }

    /// Consume the next token. Panics if the parser isn't currently at `kind`.
    pub(crate) fn bump(&mut self, kind: SyntaxKind) {
        assert!(self.eat(kind));
    }

    /// Advances the parser by one token
    pub(crate) fn bump_any(&mut self) {
        let kind = self.nth(0);
        if kind == SyntaxKind::EOF {
            return;
        }
        self.do_bump(kind, 1);
    }

    /// Advances the parser by one token, remapping its kind.
    /// This is useful to create contextual keywords from
    /// identifiers. For example, the lexer creates a `union`
    /// *identifier* token, but the parser remaps it to the
    /// `union` keyword, and keyword is what ends up in the
    /// final tree.
    pub(crate) fn bump_remap(&mut self, kind: SyntaxKind) {
        if self.nth(0) == SyntaxKind::EOF {
            // FIXME: panic!?
            return;
        }
        self.do_bump(kind, 1);
    }

    /// Emit error with the `message`
    /// FIXME: this should be much more fancy and support
    /// structured errors with spans and notes, like rustc
    /// does.
    pub(crate) fn error<T: Into<String>>(&mut self, message: T) {
        let msg = message.into();
        self.push_event(Event::Error { msg });
    }

    /// Consume the next token if it is `kind` or emit an error
    /// otherwise.
    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        if self.eat(kind) {
            return true;
        }
        self.error(format!("expected {kind:?}"));
        false
    }

    /// Create an error node and consume the next token.
    pub(crate) fn err_and_bump(&mut self, message: &str) {
        self.err_recover(message, TokenSet::EMPTY);
    }

    /// Create an error node and consume the next token.
    pub(crate) fn err_recover(&mut self, message: &str, recovery: TokenSet) {
        if matches!(self.current(), T!['{'] | T!['}']) {
            self.error(message);
            return;
        }

        if self.at_ts(recovery) {
            self.error(message);
            return;
        }

        let m = self.start();
        self.error(message);
        self.bump_any();
        m.complete(self, SyntaxKind::Error);
    }

    fn do_bump(&mut self, kind: SyntaxKind, n_raw_tokens: u8) {
        self.pos += n_raw_tokens as usize;
        self.push_event(Event::Token { kind, n_raw_tokens });
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }
}

impl Marker {
    fn new(pos: u32) -> Marker {
        Marker {
            pos,
            bomb: DropBomb::new("Marker must be either completed or abandoned"),
        }
    }

    /// Finishes the syntax tree node and assigns `kind` to it,
    /// and mark the create a `CompletedMarker` for possible future
    /// operation like `.precede()` to deal with forward_parent.
    pub(crate) fn complete(mut self, p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();
        let idx = self.pos as usize;
        match &mut p.events[idx] {
            Event::Start { kind: slot, .. } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }
        p.push_event(Event::Finish);
        CompletedMarker::new(self.pos, kind)
    }

    /// Abandons the syntax tree node. All its children
    /// are attached to its parent instead.
    pub(crate) fn abandon(mut self, p: &mut Parser<'_>) {
        self.bomb.defuse();
        let idx = self.pos as usize;
        if idx == p.events.len() - 1 {
            match p.events.pop() {
                Some(Event::Start {
                    kind: SyntaxKind::Tombstone,
                    forward_parent: None,
                }) => (),
                _ => unreachable!(),
            }
        }
    }
}

pub(crate) struct CompletedMarker {
    pos: u32,
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(pos: u32, kind: SyntaxKind) -> Self {
        CompletedMarker { pos, kind }
    }

    /// This method allows to create a new node which starts
    /// *before* the current one. That is, parser could start
    /// node `A`, then complete it, and then after parsing the
    /// whole `A`, decide that it should have started some node
    /// `B` before starting `A`. `precede` allows to do exactly
    /// that. See also docs about
    /// [`Event::Start::forward_parent`](crate::event::Event::Start::forward_parent).
    ///
    /// Given completed events `[START, FINISH]` and its corresponding
    /// `CompletedMarker(pos: 0, _)`.
    /// Append a new `START` events as `[START, FINISH, NEWSTART]`,
    /// then mark `NEWSTART` as `START`'s parent with saving its relative
    /// distance to `NEWSTART` into forward_parent(=2 in this case);
    pub(crate) fn precede(self, p: &mut Parser<'_>) -> Marker {
        let new_pos = p.start();
        let idx = self.pos as usize;
        match &mut p.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(new_pos.pos - self.pos);
            }
            _ => unreachable!(),
        }
        new_pos
    }

    /// Extends this completed marker *to the left* up to `m`.
    pub(crate) fn extend_to(self, p: &mut Parser<'_>, mut m: Marker) -> CompletedMarker {
        m.bomb.defuse();
        let idx = m.pos as usize;
        match &mut p.events[idx] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(self.pos - m.pos);
            }
            _ => unreachable!(),
        }
        self
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }
}
