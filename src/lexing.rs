use std::{iter::Peekable, str::Chars};

use crate::{
    source_code::SourceCode,
    token::{Token, TokenKind, KEYWORD},
    util::Span,
};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src SourceCode,
    src: Peekable<Chars<'src>>,
    offset: usize,
}

#[derive(Clone, Debug)]
pub enum LexErrorType {
    Expected(char),
    UnexpectedCharacter(char),
    UncompleteNumber,
    UncompleteString,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub offset: usize,
    pub ty: LexErrorType,
    pub recoverable: Option<TokenKind>,
}

// Basic lexer functions
impl<'src> Lexer<'src> {
    pub fn new(src: &'src SourceCode) -> Self {
        Self {
            source: src,
            src: src.source().chars().peekable(),
            offset: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        let c = self.src.next()?;
        self.offset += 1;
        Some(c)
    }

    fn peek(&mut self) -> Option<char> {
        self.src.peek().cloned()
    }

    fn take_while<F>(&mut self, mut f: F) -> String
    where
        F: FnMut(char) -> bool,
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

    fn skip_while<F>(&mut self, mut f: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(c) = self.peek() {
            if !f(c) {
                break;
            }
            self.next();
        }
    }
}

type LexResult = Result<Token, LexError>;

// Atoms
impl<'src> Lexer<'src> {
    fn consume_number(&mut self) -> LexResult {
        // hex: 0x[0-9a-fA-F]+
        // dec: [0-9]+
        // float: [0-9]+.[0-9]+
        let start = self.offset;
        match self.peek() {
            Some('0') => {
                self.next();
                match self.peek() {
                    Some('x') => {
                        self.next();
                        let s = self.take_while(|c| c.is_ascii_hexdigit());
                        if s.is_empty() {
                            return Err(LexError {
                                offset: self.offset,
                                ty: LexErrorType::UncompleteNumber,
                                recoverable: None,
                            });
                        }
                        let end = self.offset;
                        let span = Span::new(start.into(), end.into());

                        Ok(Token::new(span, i64::from_str_radix(&s, 16).unwrap()))
                    }
                    _ => {
                        let s = self.take_while(|c| c.is_ascii_digit());
                        if s.is_empty() {
                            return Err(LexError {
                                offset: self.offset,
                                ty: LexErrorType::UncompleteNumber,
                                recoverable: None,
                            });
                        }
                        let end = self.offset;
                        let span = Span::new(start.into(), end.into());
                        Ok(Token::new(span, s.parse::<i64>().unwrap()))
                    }
                }
            }
            _ => {
                let s = self.take_while(|c| c.is_ascii_digit());
                if s.is_empty() {
                    return Err(LexError {
                        offset: self.offset,
                        ty: LexErrorType::UncompleteNumber,
                        recoverable: None,
                    });
                }
                if self.peek() == Some('.') {
                    self.next();
                    let s = format!("{}.{}", s, self.take_while(|c| c.is_ascii_digit()));
                    let end = self.offset;
                    let span = Span::new(start.into(), end.into());
                    Ok(Token::new(span, s.parse::<f64>().unwrap()))
                } else {
                    let end = self.offset;
                    let span = Span::new(start.into(), end.into());
                    Ok(Token::new(span, s.parse::<i64>().unwrap()))
                }
            }
        }
    }

    fn consume_ident(&mut self) -> LexResult {
        let start = self.offset;
        let s = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let end = self.offset;
        let span = Span::new(start.into(), end.into());
        if KEYWORD.contains(&s.as_str()) {
            Ok(Token::new(span, TokenKind::Keyword(s)))
        } else {
            Ok(Token::new(span, TokenKind::Identifier(s)))
        }
    }

    fn consume_string(&mut self) -> LexResult {
        let start = self.offset;
        let mut s = String::new();
        self.next();
        while let Some(c) = self.next() {
            match c {
                '"' => break,
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
        let end = self.offset;
        let span = Span::new(start.into(), end.into());
        Ok(Token::new(span, s))
    }
}

#[derive(Debug, Clone)]
pub struct LexingResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
}

impl<'src> Lexer<'src> {
    pub fn lex(&mut self) -> LexingResult {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        while let Some(c) = self.peek() {
            let start = self.offset;
            let kind = match c {
                '0'..='9' => self.consume_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.consume_ident(),
                '"' => self.consume_string(),
                ',' | '.' | '|' | '!' | '$' | '=' | '(' | ')' | '[' | ']' | '{' | '}' => {
                    self.next();
                    Ok(Token::new(Span::new(start.into(), self.offset.into()), c))
                }
                '@' => {
                    self.next();
                    match self.peek() {
                        Some('@') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::AtAt,
                            ))
                        }
                        _ => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::Expected('@'),
                            recoverable: Some(TokenKind::AtAt),
                        }),
                    }
                }
                ':' => {
                    self.next();
                    match self.peek() {
                        Some('-') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::ColonDash,
                            ))
                        }
                        _ => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::Expected('-'),
                            recoverable: Some(TokenKind::ColonDash),
                        }),
                    }
                }
                ' ' | '\t' | '\r' | '\n' => {
                    self.next();
                    continue;
                }
                _ => {
                    self.next();
                    Err(LexError {
                        offset: self.offset,
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
        LexingResult { tokens, errors }
    }

    pub fn report_error(&self, err: LexError) {
        let (line, col) = self.source.line_col(err.offset);
        match err.ty {
            LexErrorType::Expected(c) => {
                eprintln!("{}:{}:{}: expected '{}'", self.source.name(), line, col, c);
            }
            LexErrorType::UnexpectedCharacter(c) => {
                eprintln!(
                    "{}:{}:{}: unexpected character '{}'",
                    self.source.name(),
                    line,
                    col,
                    c
                );
            }
            LexErrorType::UncompleteNumber => {
                eprintln!("{}:{}:{}: uncomplete number", self.source.name(), line, col);
            }
            LexErrorType::UncompleteString => {
                eprintln!("{}:{}:{}: uncomplete string", self.source.name(), line, col);
            }
        }
    }

    pub fn try_recover(&mut self, err: LexError) {
        todo!()
    }
}

#[test]
fn test_lexing_number() {
    let source = SourceCode::phony("0x1234".to_owned());
    let mut lexer = Lexer::new(&source);
    assert_eq!(lexer.consume_number().unwrap().kind, TokenKind::Int(0x1234));
    let source = SourceCode::phony("1234".to_owned());
    let mut lexer = Lexer::new(&source);
    assert_eq!(lexer.consume_number().unwrap().kind, TokenKind::Int(1234));
    let source = SourceCode::phony("1234.5678".to_owned());
    let mut lexer = Lexer::new(&source);
    assert_eq!(
        lexer.consume_number().unwrap().kind,
        TokenKind::Float(1234.5678)
    );
    let source = SourceCode::phony("1234.56".to_owned());
    let mut lexer = Lexer::new(&source);
    assert_eq!(
        lexer.consume_number().unwrap().kind,
        TokenKind::Float(1234.56)
    );
    let source = SourceCode::phony("0x".to_owned());
    let mut lexer = Lexer::new(&source);
    assert!(lexer.consume_number().is_err());
    let source = SourceCode::phony("0x1234".to_owned());
    let mut lexer = Lexer::new(&source);
    assert_eq!(lexer.consume_number().unwrap().kind, TokenKind::Int(0x1234));
}

#[test]
fn test_lexing() {
    let source = r#"
    a, b, c.
    hello(X) :- X = 1.
    test @@ 1234 : b,c.
    "#;

    let source = SourceCode::phony(source.to_owned());
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    assert_eq!(result.errors.len(), 1);
    assert_eq!(result.tokens.len(), 21);
}
