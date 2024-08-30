use crate::{
    frontend::token::{Operator, Token, TokenKind},
    util::{Pos, Source, Span},
};

/// A lexer for LMNtal.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    chars: Vec<char>,
    src: &'src Source,
    offset: usize,
    warnings: Vec<LexWarning>,
}

#[derive(Clone, Debug)]
pub enum LexErrorType {
    Expected(char),
    /// Unexpected character with the character
    UnexpectedCharacter(char),
    /// Unclosed bracket with the character and the offset of the pair
    UnmatchedBracket(char, Pos),
    UncompleteNumber,
    UncompleteString,
    UnclosedQuote,
    UnclosedComment,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub pos: Pos,
    pub ty: LexErrorType,
    pub recoverable: Option<(TokenKind, usize)>,
}

#[derive(Clone, Debug)]
pub enum LexWarningType {
    StringInsideCharLiteral,
}

#[derive(Clone, Debug)]
pub struct LexWarning {
    pub pos: Pos,
    pub ty: LexWarningType,
}

#[derive(Debug, Clone)]
pub struct LexingResult {
    pub tokens: Vec<Token>,
    pub warnings: Vec<LexWarning>,
    pub errors: Vec<LexError>,
}

impl LexError {
    pub fn try_recover(&self, kind: TokenKind) -> bool {
        match &self.recoverable {
            Some(k) => k.0 == kind,
            None => false,
        }
    }

    pub fn to_string(&self, source: &Source) -> String {
        let (line, col) = self.pos.line_col();
        match &self.ty {
            LexErrorType::Expected(c) => {
                format!("{}:{}:{}: expected '{}'", source.name(), line, col, c)
            }
            LexErrorType::UnexpectedCharacter(c) => {
                format!(
                    "{}:{}:{}: unexpected character '{}'",
                    source.name(),
                    line,
                    col,
                    c
                )
            }
            LexErrorType::UncompleteNumber => {
                format!("{}:{}:{}: uncomplete number", source.name(), line, col)
            }
            LexErrorType::UncompleteString => {
                format!("{}:{}:{}: uncomplete string", source.name(), line, col)
            }
            LexErrorType::UnclosedQuote => {
                format!("{}:{}:{}: unclosed quote", source.name(), line, col)
            }
            LexErrorType::UnclosedComment => {
                format!("{}:{}:{}: unclosed comment", source.name(), line, col)
            }
            LexErrorType::UnmatchedBracket(c, offset) => {
                let (pair_line, pair_col) = offset.line_col();
                format!(
                    "{}:{}:{}: unclosed bracket '{}'",
                    source.name(),
                    pair_line,
                    pair_col,
                    c,
                )
            }
        }
    }
}

// Basic lexer functions
impl<'src> Lexer<'src> {
    pub fn new(src: &'src Source) -> Self {
        Self {
            src,
            chars: src.source().chars().collect(),
            offset: 0,
            warnings: Vec::new(),
        }
    }

    fn cur_pos(&self) -> Pos {
        let (line, column) = self.src.line_col(self.offset);
        Pos::new(self.offset as u32, line as u32, column as u32)
    }

    fn next(&mut self) -> Option<char> {
        if self.offset >= self.chars.len() {
            None
        } else {
            let c = self.chars[self.offset];
            self.offset += 1;
            Some(c)
        }
    }

    fn peek(&mut self) -> Option<char> {
        if self.offset >= self.chars.len() {
            None
        } else {
            Some(self.chars[self.offset])
        }
    }

    fn skip<F>(&mut self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some(c) = self.peek() {
            if f(c) {
                self.advance(1);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn advance(&mut self, n: usize) {
        self.offset += n;
    }

    fn rewind(&mut self, n: usize) {
        self.offset -= n;
    }

    fn take_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if !f(c) {
                break;
            }
            s.push(self.next().unwrap());
        }
        s
    }

    fn slice(&self, span: Span) -> &str {
        &self.src.source()[span.low().offset as usize..span.high().offset as usize]
    }

    fn sequence(&mut self, s: &[char]) -> bool {
        let rollback = self.offset;
        for c in s {
            if self.next() != Some(*c) {
                self.offset = rollback;
                return false;
            }
        }
        true
    }

    /// Consume a quoted string, return the span of the content (without quotes).
    fn quoted(&mut self, left: &str, right: &str) -> Option<Span> {
        let left = left.chars().collect::<Vec<_>>();
        let right = right.chars().collect::<Vec<_>>();
        let rollback = self.offset;
        if !self.sequence(&left) {
            return None;
        }
        let start = self.cur_pos();
        let mut escape_flag = false;
        while let Some(c) = self.next() {
            if c == '\n' {
                // newline is not allowed in a quoted string
                // TODO: report an error
                break;
            }
            if escape_flag {
                escape_flag = false;
                continue;
            }
            if c == '\\' {
                escape_flag = true;
                continue;
            }
            // sequence(1) will consume the right quote so we need to get the current position before it
            let end = self.cur_pos();
            if self.sequence(&right) {
                return Some(Span::new(start, end));
            }
        }
        self.offset = rollback;
        None
    }
}

type LexResult = Result<Token, LexError>;

// Atoms
impl<'src> Lexer<'src> {
    fn consume_number(&mut self) -> LexResult {
        // hex: 0x[0-9a-fA-F]+
        // dec: [0-9]+
        // float: [0-9]+.[0-9]+
        let start = self.cur_pos();
        match self.peek() {
            Some('0') => {
                self.advance(1);
                match self.peek() {
                    Some('x') => {
                        self.advance(1);
                        let s = self.take_while(|c| c.is_ascii_hexdigit());
                        if s.is_empty() {
                            return Err(LexError {
                                pos: self.cur_pos(),
                                ty: LexErrorType::UncompleteNumber,
                                recoverable: None,
                            });
                        }
                        let end = self.cur_pos();
                        let span = Span::new(start, end);

                        Ok(Token::new(
                            span,
                            TokenKind::new_number(i64::from_str_radix(&s, 16).unwrap(), 16),
                        ))
                    }
                    Some('b') => {
                        self.advance(1);
                        let s = self.take_while(|c| c == '0' || c == '1');
                        if s.is_empty() {
                            return Err(LexError {
                                pos: self.cur_pos(),
                                ty: LexErrorType::UncompleteNumber,
                                recoverable: None,
                            });
                        }
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        Ok(Token::new(
                            span,
                            TokenKind::new_number(i64::from_str_radix(&s, 2).unwrap(), 2),
                        ))
                    }
                    Some('o') => {
                        self.advance(1);
                        let s = self.take_while(|c| c.is_ascii_digit() && c != '8' && c != '9');
                        if s.is_empty() {
                            return Err(LexError {
                                pos: self.cur_pos(),
                                ty: LexErrorType::UncompleteNumber,
                                recoverable: None,
                            });
                        }
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        Ok(Token::new(
                            span,
                            TokenKind::new_number(i64::from_str_radix(&s, 8).unwrap(), 8),
                        ))
                    }
                    Some('.') => {
                        self.advance(1);
                        let frac = self.take_while(|c| c.is_ascii_digit());
                        if frac.is_empty() {
                            // the dot may be end of a process list
                            self.rewind(1); // rewind the dot
                            return Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::new_number(0, 10),
                            ));
                        }
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        let s = format!("0.{}", frac);
                        Ok(Token::new(span, s.parse::<f64>().unwrap()))
                    }
                    _ => {
                        if let Some(c) = self.peek() {
                            if c.is_ascii_alphabetic() {
                                return Err(LexError {
                                    pos: self.cur_pos(),
                                    ty: LexErrorType::UncompleteNumber,
                                    recoverable: None,
                                });
                            }
                        }
                        let s = self.take_while(|c| c.is_ascii_digit());
                        if s.is_empty() {
                            return Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::new_number(0, 10),
                            ));
                        }
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        Ok(Token::new(
                            span,
                            TokenKind::new_number(s.parse::<i64>().unwrap(), 10),
                        ))
                    }
                }
            }
            Some('.') => {
                self.advance(1);
                let frac = self.take_while(|c| c.is_ascii_digit());
                if frac.is_empty() {
                    Ok(Token::new(
                        Span::new(start, self.cur_pos()),
                        TokenKind::Period,
                    ))
                } else {
                    let end = self.cur_pos();
                    let span = Span::new(start, end);
                    let s = format!("0.{}", frac);
                    Ok(Token::new(span, s.parse::<f64>().unwrap()))
                }
            }
            _ => {
                let s = self.take_while(|c| c.is_ascii_digit());
                if s.is_empty() {
                    return Err(LexError {
                        pos: self.cur_pos(),
                        ty: LexErrorType::UncompleteNumber,
                        recoverable: None,
                    });
                }
                if self.peek() == Some('.') {
                    self.advance(1);
                    let frac = self.take_while(|c| c.is_ascii_digit());
                    let end = self.cur_pos();
                    let span = Span::new(start, end);
                    if frac.is_empty() {
                        self.rewind(1);
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        Ok(Token::new(
                            span,
                            TokenKind::new_number(s.parse::<i64>().unwrap(), 10),
                        ))
                    } else {
                        let s = format!("{}.{}", s, frac);
                        Ok(Token::new(span, s.parse::<f64>().unwrap()))
                    }
                } else {
                    let end = self.cur_pos();
                    let span = Span::new(start, end);
                    Ok(Token::new(
                        span,
                        TokenKind::new_number(s.parse::<i64>().unwrap(), 10),
                    ))
                }
            }
        }
    }

    fn consume_atom_name(&mut self) -> LexResult {
        let start = self.cur_pos();
        let s = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let mut path = vec![s.clone()];
        while self.skip(|c| c == '.') {
            let next = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
            if next.is_empty() {
                self.rewind(1);
                break;
            }
            path.push(next);
        }
        if path.len() > 1 {
            let end = self.cur_pos();
            let span = Span::new(start, end);
            Ok(Token::new(span, TokenKind::PathedAtomName(path)))
        } else if let Some(op) = Operator::parse(&s) {
            let end = self.cur_pos();
            let span = Span::new(start, end);
            Ok(Token::new(span, TokenKind::Operator(op)))
        } else {
            let end = self.cur_pos();
            let span = Span::new(start, end);
            Ok(Token::new(span, TokenKind::AtomName(s)))
        }
    }

    fn consume_link_name(&mut self) -> LexResult {
        let start = self.cur_pos();
        let s = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let end = self.cur_pos();
        let span = Span::new(start, end);
        Ok(Token::new(span, TokenKind::LinkName(s)))
    }

    /// Consume a symbol name, e.g. `'abc'` or `'ab''cd'`.
    fn consume_symbol_name(&mut self) -> LexResult {
        let start = self.cur_pos();
        let mut end = start;
        let mut inner_start = start;
        let mut inner_end = start;

        while let Some(span) = self.quoted("'", "'") {
            if end == start {
                inner_start = span.low(); // update the inner start position if it's the first time
            }
            end = self.cur_pos(); // extend the end position
            inner_end = span.high(); // update the inner end position
        }

        if end == start {
            return Err(LexError {
                pos: self.cur_pos(),
                ty: LexErrorType::UnclosedQuote,
                recoverable: None,
            });
        }

        let span = Span::new(start, end);
        let content = Span::new(inner_start, inner_end);
        Ok(Token::new(
            span,
            TokenKind::SymbolName(self.slice(content).to_owned()),
        ))
    }

    /// Consume a character literal, e.g. `#'a'`.
    ///
    /// If there is more than one character, it will be an error.
    fn consume_char(&mut self) -> LexResult {
        // skip the sharp sign
        let start = self.cur_pos();
        self.advance(1);
        if let Some(span) = self.quoted("\"", "\"") {
            let end = self.cur_pos();
            let whole_span = Span::new(start, end);
            let s = self.slice(span);
            if let Some(c) = is_char(s) {
                Ok(Token::new(whole_span, TokenKind::Char(c)))
            } else {
                Err(LexError {
                    pos: self.cur_pos(),
                    ty: LexErrorType::UnexpectedCharacter(s.chars().next().unwrap()),
                    recoverable: None,
                })
            }
        } else {
            Err(LexError {
                pos: self.cur_pos(),
                ty: LexErrorType::UnclosedQuote,
                recoverable: None,
            })
        }
    }

    fn consume_string(&mut self) -> LexResult {
        let start = self.cur_pos();
        let mut s = String::new();
        let mut terminated = false;
        self.advance(1);
        while let Some(c) = self.next() {
            match c {
                '"' => {
                    terminated = true;
                    break;
                }
                '\\' => {
                    let c = self.next().unwrap();
                    match c {
                        'n' => s.push('\n'),
                        'r' => s.push('\r'),
                        't' => s.push('\t'),
                        '0' => s.push('\0'),
                        _ => s.push(c),
                    }
                }
                _ => s.push(c),
            }
        }
        if !terminated {
            return Err(LexError {
                pos: self.cur_pos(),
                ty: LexErrorType::UncompleteString,
                recoverable: None,
            });
        }
        let end = self.cur_pos();
        let span = Span::new(start, end);
        Ok(Token::new(span, s))
    }
}

impl<'src> Lexer<'src> {
    pub fn lex(mut self) -> LexingResult {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut bracket_stack = Vec::new();
        while let Some(c) = self.peek() {
            let start = self.cur_pos();
            let kind = match c {
                '0'..='9' | '.' => self.consume_number(),
                'a'..='z' => self.consume_atom_name(),
                'A'..='Z' | '_' => self.consume_link_name(),
                '#' => self.consume_char(),
                '"' => self.consume_string(),
                '\'' => self.consume_symbol_name(),
                ',' | '|' | '!' | '$' | '~' => {
                    self.advance(1);
                    Ok(Token::new(Span::new(start, self.cur_pos()), c))
                }
                '(' => {
                    self.advance(1);
                    bracket_stack.push((start, '('));
                    Ok(Token::new(Span::new(start, self.cur_pos()), '('))
                }
                '[' => {
                    self.advance(1);
                    if let Some(':') = self.peek() {
                        self.rewind(1);
                        if let Some(quoted) = self.quoted("[:", ":]") {
                            let quoted = self.slice(quoted).to_owned();
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Quoted(quoted),
                            ))
                        } else {
                            Err(LexError {
                                pos: self.cur_pos(),
                                ty: LexErrorType::UnclosedQuote,
                                recoverable: None,
                            })
                        }
                    } else {
                        bracket_stack.push((start, '['));
                        Ok(Token::new(Span::new(start, self.cur_pos()), '['))
                    }
                }
                '{' => {
                    self.advance(1);
                    bracket_stack.push((start, '{'));
                    Ok(Token::new(Span::new(start, self.cur_pos()), '{'))
                }
                ')' => {
                    self.advance(1);
                    match bracket_stack.pop() {
                        Some((_, '(')) => Ok(Token::new(Span::new(start, self.cur_pos()), ')')),
                        Some((pair, _)) => Err(LexError {
                            pos: self.cur_pos(),
                            ty: LexErrorType::UnmatchedBracket('(', pair),
                            recoverable: None,
                        }),
                        _ => Err(LexError {
                            pos: self.cur_pos(),
                            ty: LexErrorType::UnmatchedBracket('(', Pos::default()),
                            recoverable: None,
                        }),
                    }
                }
                ']' => {
                    self.advance(1);
                    match bracket_stack.pop() {
                        Some((_, '[')) => Ok(Token::new(Span::new(start, self.cur_pos()), ']')),
                        Some((pair, _)) => Err(LexError {
                            pos: self.cur_pos(),
                            ty: LexErrorType::UnmatchedBracket('[', pair),
                            recoverable: None,
                        }),
                        _ => Err(LexError {
                            pos: self.cur_pos(),
                            ty: LexErrorType::UnmatchedBracket('[', Pos::default()),
                            recoverable: None,
                        }),
                    }
                }
                '}' => {
                    self.advance(1);
                    let ty = match self.peek() {
                        Some('/') => {
                            self.advance(1);
                            match self.peek() {
                                Some('@') => {
                                    self.advance(1);
                                    TokenKind::RBraceSlashAt
                                }
                                _ => TokenKind::RBraceSlash,
                            }
                        }
                        Some('_') => {
                            self.advance(1);
                            match self.peek() {
                                Some('@') => {
                                    self.advance(1);
                                    TokenKind::RBraceUnderbarAt
                                }
                                Some('/') => {
                                    self.advance(1);
                                    match self.peek() {
                                        Some('@') => {
                                            self.advance(1);
                                            TokenKind::RBraceUnderbarSlashAt
                                        }
                                        _ => TokenKind::RBraceUnderbarSlash,
                                    }
                                }
                                _ => TokenKind::RBraceUnderbar,
                            }
                        }
                        Some('@') => {
                            self.advance(1);
                            TokenKind::RBraceAt
                        }
                        Some('*') => {
                            self.advance(1);
                            TokenKind::RBraceAsterisk
                        }
                        _ => TokenKind::RightBrace,
                    };
                    match bracket_stack.pop() {
                        Some((_, '{')) => Ok(Token::new(Span::new(start, self.cur_pos()), ty)),
                        Some((pair, _)) => Err(LexError {
                            pos: self.cur_pos(),
                            ty: LexErrorType::UnmatchedBracket('{', pair),
                            recoverable: None,
                        }),
                        _ => Err(LexError {
                            pos: self.cur_pos(),
                            ty: LexErrorType::UnmatchedBracket('{', Pos::default()),
                            recoverable: None,
                        }),
                    }
                }
                '@' => {
                    self.advance(1);
                    if let Some('@') = self.peek() {
                        self.advance(1);
                        Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::AtAt,
                        ))
                    } else {
                        Ok(Token::new(Span::new(start, self.cur_pos()), TokenKind::At))
                    }
                }
                ':' => {
                    self.advance(1);
                    if let Some('-') = self.peek() {
                        self.advance(1);
                        Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::ColonDash,
                        ))
                    } else {
                        Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Colon,
                        ))
                    }
                }
                c if c.is_whitespace() => {
                    self.advance(1);
                    continue;
                }
                '+' => {
                    self.advance(1);
                    match self.peek() {
                        Some('.') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FAdd),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::IAdd),
                        )),
                    }
                }
                '-' => {
                    self.advance(1);
                    match self.peek() {
                        Some('.') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FSub),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::ISub),
                        )),
                    }
                }
                '*' => {
                    self.advance(1);
                    let op = match self.peek() {
                        Some('.') => {
                            self.advance(1);
                            Operator::FMul
                        }
                        Some('*') => {
                            self.advance(1);
                            Operator::IPow
                        }
                        _ => Operator::IMul,
                    };
                    Ok(Token::new(
                        Span::new(start, self.cur_pos()),
                        TokenKind::Operator(op),
                    ))
                }
                '/' => {
                    self.advance(1);
                    match self.peek() {
                        Some('.') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FDiv),
                            ))
                        }
                        Some('/') => {
                            // line comment
                            self.advance(1);
                            self.take_while(|c| c != '\n');
                            continue;
                        }
                        Some('*') => {
                            // block comment
                            self.advance(1);
                            loop {
                                match self.next() {
                                    Some('*') => {
                                        if self.peek() == Some('/') {
                                            self.advance(1);
                                            break;
                                        }
                                    }
                                    Some(_) => continue,
                                    None => break,
                                }
                            }
                            continue;
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::IDiv),
                        )),
                    }
                }
                '%' => {
                    // line comment
                    self.advance(1);
                    self.take_while(|c| c != '\n');
                    continue;
                }
                '>' => {
                    self.advance(1);
                    match self.peek() {
                        Some('.') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FGt),
                            ))
                        }
                        Some('=') => {
                            self.advance(1);
                            match self.peek() {
                                Some('.') => {
                                    self.advance(1);
                                    Ok(Token::new(
                                        Span::new(start, self.cur_pos()),
                                        TokenKind::Operator(Operator::FGe),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start, self.cur_pos()),
                                    TokenKind::Operator(Operator::IGe),
                                )),
                            }
                        }
                        Some('+') | Some('*') => {
                            self.advance(1);
                            if let Some('<') = self.peek() {
                                self.advance(1);
                                Ok(Token::new(
                                    Span::new(start, self.cur_pos()),
                                    TokenKind::Operator(Operator::HyperlinkFuse),
                                ))
                            } else {
                                Err(LexError {
                                    pos: self.cur_pos(),
                                    ty: LexErrorType::Expected('<'),
                                    recoverable: Some((
                                        TokenKind::Operator(Operator::HyperlinkUnify),
                                        tokens.len(),
                                    )),
                                })
                            }
                        }
                        Some('<') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::HyperlinkFuse),
                            ))
                        }
                        Some('>') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::HyperlinkUnify),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::IGt),
                        )),
                    }
                }
                '<' => {
                    self.advance(1);
                    match self.peek() {
                        Some('.') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FLt),
                            ))
                        }
                        Some('=') => {
                            self.advance(1);
                            match self.peek() {
                                Some('.') => {
                                    self.advance(1);
                                    Ok(Token::new(
                                        Span::new(start, self.cur_pos()),
                                        TokenKind::Operator(Operator::FLe),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start, self.cur_pos()),
                                    TokenKind::Operator(Operator::ILe),
                                )),
                            }
                        }
                        Some('<') => {
                            self.advance(1);
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::HyperlinkUnify),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::ILt),
                        )),
                    }
                }
                '=' => {
                    self.advance(1);
                    match self.peek() {
                        Some(':') => {
                            self.advance(1);
                            match self.peek() {
                                Some('=') => {
                                    self.advance(1);
                                    match self.peek() {
                                        Some('.') => {
                                            self.advance(1);
                                            Ok(Token::new(
                                                Span::new(start, self.cur_pos()),
                                                TokenKind::Operator(Operator::FEq),
                                            ))
                                        }
                                        _ => Ok(Token::new(
                                            Span::new(start, self.cur_pos()),
                                            TokenKind::Operator(Operator::IEq),
                                        )),
                                    }
                                }
                                _ => Err(LexError {
                                    pos: self.cur_pos(),
                                    ty: LexErrorType::Expected('='),
                                    recoverable: Some((
                                        TokenKind::Operator(Operator::IEq),
                                        tokens.len(),
                                    )),
                                }),
                            }
                        }
                        Some('\\') => {
                            self.advance(1);
                            match self.peek() {
                                Some('=') => {
                                    self.advance(1);
                                    match self.peek() {
                                        Some('.') => {
                                            self.advance(1);
                                            Ok(Token::new(
                                                Span::new(start, self.cur_pos()),
                                                TokenKind::Operator(Operator::FNe),
                                            ))
                                        }
                                        _ => Ok(Token::new(
                                            Span::new(start, self.cur_pos()),
                                            TokenKind::Operator(Operator::INe),
                                        )),
                                    }
                                }
                                _ => Err(LexError {
                                    pos: self.cur_pos(),
                                    ty: LexErrorType::Expected('='),
                                    recoverable: Some((
                                        TokenKind::Operator(Operator::INe),
                                        tokens.len(),
                                    )),
                                }),
                            }
                        }
                        Some('=') => {
                            self.advance(1);
                            match self.peek() {
                                Some('=') => {
                                    self.advance(1);
                                    Ok(Token::new(
                                        Span::new(start, self.cur_pos()),
                                        TokenKind::Operator(Operator::UnaryEq),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start, self.cur_pos()),
                                    TokenKind::Operator(Operator::GroundEq),
                                )),
                            }
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::Equal),
                        )),
                    }
                }
                '\\' => {
                    self.advance(1);
                    match self.peek() {
                        Some('=') => {
                            self.advance(1);
                            match self.peek() {
                                Some('=') => {
                                    self.advance(1);
                                    Ok(Token::new(
                                        Span::new(start, self.cur_pos()),
                                        TokenKind::Operator(Operator::UnaryNe),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start, self.cur_pos()),
                                    TokenKind::Operator(Operator::GroundNe),
                                )),
                            }
                        }
                        _ => Ok(Token::new(Span::new(start, self.cur_pos()), '\\')),
                    }
                }
                _ => {
                    self.advance(1);
                    Err(LexError {
                        pos: self.cur_pos(),
                        ty: LexErrorType::UnexpectedCharacter(c),
                        recoverable: None,
                    })
                }
            };
            match kind {
                Ok(token) => tokens.push(token),
                Err(err) => errors.push(err),
            }
        }
        if !bracket_stack.is_empty() {
            for (offset, c) in bracket_stack {
                errors.push(LexError {
                    pos: offset,
                    ty: LexErrorType::UnmatchedBracket(c, Pos::default()),
                    recoverable: None,
                });
            }
        }
        LexingResult {
            tokens,
            errors,
            warnings: self.warnings,
        }
    }
}

/// Check if the character is a valid character.
///
/// Valid characters are:
/// - ASCII characters      (e.g. a, b, c, 1, 2, 3, ...)
/// - Unicode characters    (e.g. \u{XXXX})
/// - Control characters    (e.g. \n, \r, \t, \0)
/// - Escape characters     (e.g. \\, \", \', \`)
fn is_char(c: &str) -> Option<char> {
    let chars = c.chars().collect::<Vec<_>>();
    if chars.len() == 1 && chars[0] != '\\' {
        return Some(chars[0]);
    }
    if chars.len() == 2 && chars[0] == '\\' {
        match chars[1] {
            // ASCII escape characters
            'n' => Some('\n'),
            'r' => Some('\r'),
            't' => Some('\t'),
            '\\' => Some('\\'),
            '0' => Some('\0'),

            // Quote characters
            '"' => Some('"'),
            '\'' => Some('\''),
            _ => None,
        }
    } else if chars.len() == 4 && chars[0] == '\\' {
        // ASCII escape characters (e.g. \x7f)
        if let Ok(c) = u32::from_str_radix(&c[1..], 16) {
            if let Some(c) = std::char::from_u32(c) {
                return Some(c);
            }
        }
        None
    } else if c.starts_with("\\u{") && c.ends_with('}') {
        let c = c.trim_start_matches("\\u{").trim_end_matches('}');
        if let Ok(c) = u32::from_str_radix(c, 16) {
            if let Some(c) = std::char::from_u32(c) {
                return Some(c);
            }
        }
        None
    } else {
        None
    }
}

#[test]
fn test_is_char() {
    assert!(is_char("a").is_some());
    assert!(is_char("1").is_some());
    assert!(is_char("\u{3042}").is_some());
    assert!(is_char("\n").is_some());
    assert!(is_char("\\n").is_some());
    assert!(is_char("\\u{3042}").is_some());
    assert!(is_char("\\u{3z42}").is_none());
    assert!(is_char("ab").is_none());
    assert!(is_char("\\").is_none());
    assert!(is_char("\\u").is_none());
}

#[test]
fn test_lexing_number() {
    macro_rules! test_number {
        ($src:expr, $kind:expr) => {
            let source = Source::from_string($src.to_owned());
            let mut lexer = Lexer::new(&source);
            assert_eq!(lexer.consume_number().unwrap().kind, $kind);
        };
    }

    macro_rules! wrong {
        ($src:expr) => {
            let source = Source::from_string($src.to_owned());
            let mut lexer = Lexer::new(&source);
            assert!(lexer.consume_number().is_err());
        };
    }

    use crate::frontend::token::Number;

    test_number!("0".to_owned(), TokenKind::Number(Number::Decimal(0)));
    test_number!("0.".to_owned(), TokenKind::Number(Number::Decimal(0))); // the dot is not consumed
    test_number!("0b101".to_owned(), TokenKind::Number(Number::Binary(5)));
    test_number!("0o123".to_owned(), TokenKind::Number(Number::Octal(83)));
    test_number!(
        "0x1234".to_owned(),
        TokenKind::Number(Number::Hexadecimal(0x1234))
    );
    test_number!("1234".to_owned(), TokenKind::Number(Number::Decimal(1234)));
    test_number!(
        "1234.5678".to_owned(),
        TokenKind::Number(Number::Float(1234.5678))
    );

    wrong!("0x");
}

#[test]
fn test_lexing() {
    // legal tokens but invalid syntax
    let source = r#"
    a, b, c.
    hello(X) :- X = 1.
    test @@ 1234 : b,c.
    "#;

    let source = Source::from_string(source.to_owned());
    let lexer = Lexer::new(&source);
    let result = lexer.lex();
    assert_eq!(result.errors.len(), 0);
    assert_eq!(result.tokens.len(), 23);
}

#[test]
fn test_lexing_operator() {
    let source = Source::from_string("#\"a\" + b *. c =:= d lsh e".to_owned());
    let lexer = Lexer::new(&source);
    let result = lexer.lex();
    assert_eq!(result.errors.len(), 0);
    assert_eq!(result.tokens.len(), 9);
    assert_eq!(result.tokens[0].kind, TokenKind::Char('a'));
    assert_eq!(result.tokens[1].kind, TokenKind::Operator(Operator::IAdd));
    assert_eq!(result.tokens[2].kind, TokenKind::AtomName("b".to_owned()));
    assert_eq!(result.tokens[3].kind, TokenKind::Operator(Operator::FMul));
    assert_eq!(result.tokens[4].kind, TokenKind::AtomName("c".to_owned()));
    assert_eq!(result.tokens[5].kind, TokenKind::Operator(Operator::IEq));
    assert_eq!(result.tokens[6].kind, TokenKind::AtomName("d".to_owned()));
    assert_eq!(
        result.tokens[7].kind,
        TokenKind::Operator(Operator::LogicalShift)
    );
    assert_eq!(result.tokens[8].kind, TokenKind::AtomName("e".to_owned()));
}
