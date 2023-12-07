use std::{iter::Peekable, str::Chars};

use crate::{
    source_code::SourceCode,
    token::{Token, TokenKind, KEYWORD},
    util::Span,
};

/// A lexer for LMNtal.
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
    UnclosedQuote,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub offset: usize,
    pub ty: LexErrorType,
    pub recoverable: Option<(TokenKind, usize)>,
}

impl LexError {
    pub fn try_recover(&self, kind: TokenKind) -> bool {
        match &self.recoverable {
            Some(k) => k.0 == kind,
            None => false,
        }
    }

    pub fn to_string(&self, source: &SourceCode) -> String {
        let (line, col) = source.line_col(self.offset);
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
        }
    }
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
        self.offset += 1;
        self.src.next()
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
                    let frac = self.take_while(|c| c.is_ascii_digit());
                    let end = self.offset;
                    let span = Span::new(start.into(), end.into());
                    if frac.is_empty() {
                        // treat as integer and remain the dot
                        Ok(Token::new(span, s.parse::<i64>().unwrap()))
                    } else {
                        let s = format!("{}.{}", s, frac);
                        Ok(Token::new(span, s.parse::<f64>().unwrap()))
                    }
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

    fn consume_char(&mut self) -> LexResult {
        self.next();
        let start = self.offset;
        match self.next() {
            Some(c) => {
                if self.next() != Some('\'') {
                    return Err(LexError {
                        offset: self.offset,
                        ty: LexErrorType::UnclosedQuote,
                        recoverable: None,
                    });
                }
                let end = self.offset;
                let span = Span::new(start.into(), end.into());
                Ok(Token::new(span, TokenKind::Char(c)))
            }
            None => Err(LexError {
                offset: self.offset,
                ty: LexErrorType::UnclosedQuote,
                recoverable: None,
            }),
        }
    }

    fn consume_string(&mut self) -> LexResult {
        let start = self.offset;
        let mut s = String::new();
        let mut terminated = false;
        self.next();
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
                offset: self.offset,
                ty: LexErrorType::UncompleteString,
                recoverable: None,
            });
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
        let mut bracket_stack = Vec::new();
        while let Some(c) = self.peek() {
            let start = self.offset;
            let kind = match c {
                '0'..='9' => self.consume_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.consume_ident(),
                '"' => self.consume_string(),
                '\'' => self.consume_char(),
                ',' | '.' | '|' | '!' | '$' => {
                    self.next();
                    Ok(Token::new(Span::new(start.into(), self.offset.into()), c))
                }
                '(' => {
                    self.next();
                    bracket_stack.push((start, '('));
                    Ok(Token::new(Span::new(start.into(), self.offset.into()), '('))
                }
                '[' => {
                    self.next();
                    bracket_stack.push((start, '['));
                    Ok(Token::new(Span::new(start.into(), self.offset.into()), '['))
                }
                '{' => {
                    self.next();
                    bracket_stack.push((start, '{'));
                    Ok(Token::new(Span::new(start.into(), self.offset.into()), '{'))
                }
                ')' => {
                    self.next();
                    match bracket_stack.pop() {
                        Some((_, '(')) => {
                            Ok(Token::new(Span::new(start.into(), self.offset.into()), ')'))
                        }
                        _ => {
                            let (line, col) = self.source.line_col(start);
                            panic!("Unbalanced parentheses at {}:{}", line, col)
                        }
                    }
                }
                ']' => {
                    self.next();
                    match bracket_stack.pop() {
                        Some((_, '[')) => {
                            Ok(Token::new(Span::new(start.into(), self.offset.into()), ']'))
                        }
                        _ => {
                            let (line, col) = self.source.line_col(start);
                            panic!("Unbalanced brackets at {}:{}", line, col)
                        }
                    }
                }
                '}' => {
                    self.next();
                    match bracket_stack.pop() {
                        Some((_, '{')) => {
                            Ok(Token::new(Span::new(start.into(), self.offset.into()), '}'))
                        }
                        _ => {
                            let (line, col) = self.source.line_col(start);
                            panic!("Unbalanced braces at {}:{}", line, col);
                        }
                    }
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
                            recoverable: Some((TokenKind::AtAt, tokens.len())),
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
                            recoverable: Some((TokenKind::ColonDash, tokens.len())),
                        }),
                    }
                }
                c if c.is_whitespace() => {
                    self.next();
                    continue;
                }
                '+' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::FAdd,
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::IAdd,
                        )),
                    }
                }
                '-' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::FSub,
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::ISub,
                        )),
                    }
                }
                '*' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::FMul,
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::IMul,
                        )),
                    }
                }
                '/' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::FDiv,
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::IDiv,
                        )),
                    }
                }
                '%' => {
                    self.next();
                    Ok(Token::new(
                        Span::new(start.into(), self.offset.into()),
                        TokenKind::IMod,
                    ))
                }
                '>' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::FGt,
                            ))
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('.') => {
                                    self.next();
                                    Ok(Token::new(
                                        Span::new(start.into(), self.offset.into()),
                                        TokenKind::FGe,
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::IGe,
                                )),
                            }
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::IGt,
                        )),
                    }
                }
                '<' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::FLt,
                            ))
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('.') => {
                                    self.next();
                                    Ok(Token::new(
                                        Span::new(start.into(), self.offset.into()),
                                        TokenKind::FLe,
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::ILe,
                                )),
                            }
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::ILt,
                        )),
                    }
                }
                '=' => {
                    self.next();
                    match self.peek() {
                        Some(':') => {
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
                                    match self.peek() {
                                        Some('.') => {
                                            self.next();
                                            Ok(Token::new(
                                                Span::new(start.into(), self.offset.into()),
                                                TokenKind::FEq,
                                            ))
                                        }
                                        _ => Ok(Token::new(
                                            Span::new(start.into(), self.offset.into()),
                                            TokenKind::IEq,
                                        )),
                                    }
                                }
                                _ => Err(LexError {
                                    offset: self.offset,
                                    ty: LexErrorType::Expected('='),
                                    recoverable: Some((TokenKind::IEq, tokens.len())),
                                }),
                            }
                        }
                        Some('\\') => {
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
                                    match self.peek() {
                                        Some('.') => {
                                            self.next();
                                            Ok(Token::new(
                                                Span::new(start.into(), self.offset.into()),
                                                TokenKind::FNe,
                                            ))
                                        }
                                        _ => Ok(Token::new(
                                            Span::new(start.into(), self.offset.into()),
                                            TokenKind::INe,
                                        )),
                                    }
                                }
                                _ => Err(LexError {
                                    offset: self.offset,
                                    ty: LexErrorType::Expected('='),
                                    recoverable: Some((TokenKind::INe, tokens.len())),
                                }),
                            }
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
                                    Ok(Token::new(
                                        Span::new(start.into(), self.offset.into()),
                                        TokenKind::UnaryEq,
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::GroundEq,
                                )),
                            }
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Equal,
                        )),
                    }
                }
                '\\' => {
                    self.next();
                    match self.peek() {
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
                                    Ok(Token::new(
                                        Span::new(start.into(), self.offset.into()),
                                        TokenKind::UnaryNe,
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::GroundNe,
                                )),
                            }
                        }
                        _ => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::Expected('='),
                            recoverable: Some((TokenKind::GroundNe, tokens.len())),
                        }),
                    }
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
            LexErrorType::UnclosedQuote => {
                eprintln!("{}:{}:{}: unclosed quote", self.source.name(), line, col);
            }
        }
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

#[test]
fn test_lexing_operator() {
    let source = SourceCode::phony("'a' + b *. c =:= d".to_owned());
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    assert_eq!(result.errors.len(), 0);
    assert_eq!(result.tokens.len(), 7);
    assert_eq!(result.tokens[0].kind, TokenKind::Char('a'));
    assert_eq!(result.tokens[1].kind, TokenKind::IAdd);
    assert_eq!(result.tokens[2].kind, TokenKind::Identifier("b".to_owned()));
    assert_eq!(result.tokens[3].kind, TokenKind::FMul);
    assert_eq!(result.tokens[4].kind, TokenKind::Identifier("c".to_owned()));
    assert_eq!(result.tokens[5].kind, TokenKind::IEq);
    assert_eq!(result.tokens[6].kind, TokenKind::Identifier("d".to_owned()));
}
