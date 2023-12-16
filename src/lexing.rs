use std::{iter::Peekable, str::Chars};

use crate::{
    token::{Operator, Token, TokenKind, KEYWORD},
    util::{SourceCode, Span},
};

/// A lexer for LMNtal.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    src: Peekable<Chars<'src>>,
    offset: usize,
}

#[derive(Clone, Debug)]
pub enum LexErrorType {
    Expected(char),
    /// Unexpected character with the character
    UnexpectedCharacter(char),
    /// Unclosed bracket with the character and the offset of the pair
    UnmatchedBracket(char, usize),
    UncompleteNumber,
    UncompleteString,
    UnclosedQuote,
    UnclosedComment,
}

#[derive(Clone, Debug)]
pub struct LexError {
    pub offset: usize,
    pub ty: LexErrorType,
    pub recoverable: Option<(TokenKind, usize)>,
}

#[derive(Debug, Clone)]
pub struct LexingResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<LexError>,
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
            LexErrorType::UnclosedComment => {
                format!("{}:{}:{}: unclosed comment", source.name(), line, col)
            }
            LexErrorType::UnmatchedBracket(c, offset) => {
                let (pair_line, pair_col) = source.line_col(*offset);
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
    pub fn new(src: &'src SourceCode) -> Self {
        Self {
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
                    Some('.') => {
                        self.next();
                        let frac = self.take_while(|c| c.is_ascii_digit());
                        if frac.is_empty() {
                            // the dot may be end of a process list
                            self.offset -= 1;
                            return Err(LexError {
                                offset: self.offset,
                                ty: LexErrorType::UncompleteNumber,
                                recoverable: None,
                            });
                        }
                        let end = self.offset;
                        let span = Span::new(start.into(), end.into());
                        let s = format!("0.{}", frac);
                        Ok(Token::new(span, s.parse::<f64>().unwrap()))
                    }
                    _ => {
                        if let Some(c) = self.peek() {
                            if c.is_ascii_alphabetic() {
                                return Err(LexError {
                                    offset: self.offset,
                                    ty: LexErrorType::UncompleteNumber,
                                    recoverable: None,
                                });
                            }
                        }
                        let s = self.take_while(|c| c.is_ascii_digit());
                        if s.is_empty() {
                            return Ok(Token::new(Span::new(start.into(), self.offset.into()), 0));
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
                        Some((pair, _)) => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::UnmatchedBracket('(', pair),
                            recoverable: None,
                        }),
                        _ => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::UnmatchedBracket('(', 0),
                            recoverable: None,
                        }),
                    }
                }
                ']' => {
                    self.next();
                    match bracket_stack.pop() {
                        Some((_, '[')) => {
                            Ok(Token::new(Span::new(start.into(), self.offset.into()), ']'))
                        }
                        Some((pair, _)) => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::UnmatchedBracket('[', pair),
                            recoverable: None,
                        }),
                        _ => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::UnmatchedBracket('[', 0),
                            recoverable: None,
                        }),
                    }
                }
                '}' => {
                    self.next();
                    match bracket_stack.pop() {
                        Some((_, '{')) => {
                            Ok(Token::new(Span::new(start.into(), self.offset.into()), '}'))
                        }
                        Some((pair, _)) => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::UnmatchedBracket('{', pair),
                            recoverable: None,
                        }),
                        _ => Err(LexError {
                            offset: self.offset,
                            ty: LexErrorType::UnmatchedBracket('{', 0),
                            recoverable: None,
                        }),
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
                                TokenKind::Operator(Operator::FAdd),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::IAdd),
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
                                TokenKind::Operator(Operator::FSub),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::ISub),
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
                                TokenKind::Operator(Operator::FMul),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::IMul),
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
                                TokenKind::Operator(Operator::FDiv),
                            ))
                        }
                        Some('/') => {
                            self.next();
                            self.take_while(|c| c != '\n');
                            continue;
                        }
                        Some('*') => {
                            self.next();
                            loop {
                                match self.next() {
                                    Some('*') => {
                                        if self.peek() == Some('/') {
                                            self.next();
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
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::IDiv),
                        )),
                    }
                }
                '%' => {
                    self.next();
                    Ok(Token::new(
                        Span::new(start.into(), self.offset.into()),
                        TokenKind::Operator(Operator::IMod),
                    ))
                }
                '>' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::Operator(Operator::FGt),
                            ))
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('.') => {
                                    self.next();
                                    Ok(Token::new(
                                        Span::new(start.into(), self.offset.into()),
                                        TokenKind::Operator(Operator::FGe),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::Operator(Operator::IGe),
                                )),
                            }
                        }
                        Some('+') | Some('*') => {
                            self.next();
                            if let Some('<') = self.peek() {
                                self.next();
                                Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::Operator(Operator::HyperlinkFuse),
                                ))
                            } else {
                                Err(LexError {
                                    offset: self.offset,
                                    ty: LexErrorType::Expected('<'),
                                    recoverable: Some((
                                        TokenKind::Operator(Operator::HyperlinkUnify),
                                        tokens.len(),
                                    )),
                                })
                            }
                        }
                        Some('<') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::Operator(Operator::HyperlinkFuse),
                            ))
                        }
                        Some('>') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::Operator(Operator::HyperlinkUnify),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::IGt),
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
                                TokenKind::Operator(Operator::FLt),
                            ))
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('.') => {
                                    self.next();
                                    Ok(Token::new(
                                        Span::new(start.into(), self.offset.into()),
                                        TokenKind::Operator(Operator::FLe),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::Operator(Operator::ILe),
                                )),
                            }
                        }
                        Some('<') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start.into(), self.offset.into()),
                                TokenKind::Operator(Operator::HyperlinkUnify),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::ILt),
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
                                                TokenKind::Operator(Operator::FEq),
                                            ))
                                        }
                                        _ => Ok(Token::new(
                                            Span::new(start.into(), self.offset.into()),
                                            TokenKind::Operator(Operator::IEq),
                                        )),
                                    }
                                }
                                _ => Err(LexError {
                                    offset: self.offset,
                                    ty: LexErrorType::Expected('='),
                                    recoverable: Some((
                                        TokenKind::Operator(Operator::IEq),
                                        tokens.len(),
                                    )),
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
                                                TokenKind::Operator(Operator::FNe),
                                            ))
                                        }
                                        _ => Ok(Token::new(
                                            Span::new(start.into(), self.offset.into()),
                                            TokenKind::Operator(Operator::INe),
                                        )),
                                    }
                                }
                                _ => Err(LexError {
                                    offset: self.offset,
                                    ty: LexErrorType::Expected('='),
                                    recoverable: Some((
                                        TokenKind::Operator(Operator::INe),
                                        tokens.len(),
                                    )),
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
                                        TokenKind::Operator(Operator::UnaryEq),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::Operator(Operator::GroundEq),
                                )),
                            }
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            TokenKind::Operator(Operator::Equal),
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
                                        TokenKind::Operator(Operator::UnaryNe),
                                    ))
                                }
                                _ => Ok(Token::new(
                                    Span::new(start.into(), self.offset.into()),
                                    TokenKind::Operator(Operator::GroundNe),
                                )),
                            }
                        }
                        _ => Ok(Token::new(
                            Span::new(start.into(), self.offset.into()),
                            '\\',
                        )),
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
        if !bracket_stack.is_empty() {
            for (offset, c) in bracket_stack {
                errors.push(LexError {
                    offset,
                    ty: LexErrorType::UnmatchedBracket(c, 0),
                    recoverable: None,
                });
            }
        }
        LexingResult { tokens, errors }
    }
}

#[test]
fn test_lexing_number() {
    macro_rules! test_number {
        ($src:expr, $kind:expr) => {
            let source = SourceCode::phony($src.to_owned());
            let mut lexer = Lexer::new(&source);
            assert_eq!(lexer.consume_number().unwrap().kind, $kind);
        };
    }

    macro_rules! wrong {
        ($src:expr) => {
            let source = SourceCode::phony($src.to_owned());
            let mut lexer = Lexer::new(&source);
            assert!(lexer.consume_number().is_err());
        };
    }

    test_number!("0".to_owned(), TokenKind::Int(0));
    test_number!("0x1234".to_owned(), TokenKind::Int(0x1234));
    test_number!("1234".to_owned(), TokenKind::Int(1234));
    test_number!("1234.5678".to_owned(), TokenKind::Float(1234.5678));

    wrong!("0x");
    wrong!("0.");
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
    assert_eq!(result.tokens[1].kind, TokenKind::Operator(Operator::IAdd));
    assert_eq!(result.tokens[2].kind, TokenKind::Identifier("b".to_owned()));
    assert_eq!(result.tokens[3].kind, TokenKind::Operator(Operator::FMul));
    assert_eq!(result.tokens[4].kind, TokenKind::Identifier("c".to_owned()));
    assert_eq!(result.tokens[5].kind, TokenKind::Operator(Operator::IEq));
    assert_eq!(result.tokens[6].kind, TokenKind::Identifier("d".to_owned()));
}
