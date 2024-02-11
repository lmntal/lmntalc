use crate::{
    frontend::token::{Operator, Token, TokenKind, KEYWORD},
    util::{Pos, Source, Span},
};

/// A lexer for LMNtal.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    chars: Vec<char>,
    src: &'src Source,
    offset: usize,
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

    fn rewind(&mut self, n: usize) {
        self.offset -= n;
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
        let start = self.cur_pos();
        match self.peek() {
            Some('0') => {
                self.next();
                match self.peek() {
                    Some('x') => {
                        self.next();
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

                        Ok(Token::new(span, i64::from_str_radix(&s, 16).unwrap()))
                    }
                    Some('.') => {
                        self.next();
                        let frac = self.take_while(|c| c.is_ascii_digit());
                        if frac.is_empty() {
                            // the dot may be end of a process list
                            self.rewind(1); // rewind the dot
                            return Ok(Token::new(Span::new(start, self.cur_pos()), 0));
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
                            return Ok(Token::new(Span::new(start, self.cur_pos()), 0));
                        }
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        Ok(Token::new(span, s.parse::<i64>().unwrap()))
                    }
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
                    self.next();
                    let frac = self.take_while(|c| c.is_ascii_digit());
                    let end = self.cur_pos();
                    let span = Span::new(start, end);
                    if frac.is_empty() {
                        self.rewind(1);
                        let end = self.cur_pos();
                        let span = Span::new(start, end);
                        Ok(Token::new(span, s.parse::<i64>().unwrap()))
                    } else {
                        let s = format!("{}.{}", s, frac);
                        Ok(Token::new(span, s.parse::<f64>().unwrap()))
                    }
                } else {
                    let end = self.cur_pos();
                    let span = Span::new(start, end);
                    Ok(Token::new(span, s.parse::<i64>().unwrap()))
                }
            }
        }
    }

    fn consume_ident(&mut self) -> LexResult {
        let start = self.cur_pos();
        let s = self.take_while(|c| c.is_ascii_alphanumeric() || c == '_');
        let end = self.cur_pos();
        let span = Span::new(start, end);
        if KEYWORD.contains(&s.as_str()) {
            Ok(Token::new(span, TokenKind::Keyword(s)))
        } else {
            Ok(Token::new(span, TokenKind::Identifier(s)))
        }
    }

    fn consume_char(&mut self) -> LexResult {
        self.next();
        let start = self.cur_pos();
        match self.next() {
            Some(c) => {
                if self.next() != Some('\'') {
                    return Err(LexError {
                        pos: self.cur_pos(),
                        ty: LexErrorType::UnclosedQuote,
                        recoverable: None,
                    });
                }
                let end = self.cur_pos();
                let span = Span::new(start, end);
                Ok(Token::new(span, TokenKind::Char(c)))
            }
            None => Err(LexError {
                pos: self.cur_pos(),
                ty: LexErrorType::UnclosedQuote,
                recoverable: None,
            }),
        }
    }

    fn consume_string(&mut self) -> LexResult {
        let start = self.cur_pos();
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
    pub fn lex(&mut self) -> LexingResult {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();
        let mut bracket_stack = Vec::new();
        while let Some(c) = self.peek() {
            let start = self.cur_pos();
            let kind = match c {
                '0'..='9' => self.consume_number(),
                'a'..='z' | 'A'..='Z' | '_' => self.consume_ident(),
                '"' => self.consume_string(),
                '\'' => self.consume_char(),
                ',' | '.' | '|' | '!' | '$' => {
                    self.next();
                    Ok(Token::new(Span::new(start, self.cur_pos()), c))
                }
                '(' => {
                    self.next();
                    bracket_stack.push((start, '('));
                    Ok(Token::new(Span::new(start, self.cur_pos()), '('))
                }
                '[' => {
                    self.next();
                    bracket_stack.push((start, '['));
                    Ok(Token::new(Span::new(start, self.cur_pos()), '['))
                }
                '{' => {
                    self.next();
                    bracket_stack.push((start, '{'));
                    Ok(Token::new(Span::new(start, self.cur_pos()), '{'))
                }
                ')' => {
                    self.next();
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
                    self.next();
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
                    self.next();
                    match bracket_stack.pop() {
                        Some((_, '{')) => Ok(Token::new(Span::new(start, self.cur_pos()), '}')),
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
                    self.next();
                    match self.peek() {
                        Some('@') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::AtAt,
                            ))
                        }
                        _ => Err(LexError {
                            pos: self.cur_pos(),
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
                                Span::new(start, self.cur_pos()),
                                TokenKind::ColonDash,
                            ))
                        }
                        _ => Err(LexError {
                            pos: self.cur_pos(),
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
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
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
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FMul),
                            ))
                        }
                        _ => Ok(Token::new(
                            Span::new(start, self.cur_pos()),
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
                                Span::new(start, self.cur_pos()),
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
                            Span::new(start, self.cur_pos()),
                            TokenKind::Operator(Operator::IDiv),
                        )),
                    }
                }
                '%' => {
                    self.next();
                    Ok(Token::new(
                        Span::new(start, self.cur_pos()),
                        TokenKind::Operator(Operator::IMod),
                    ))
                }
                '>' => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FGt),
                            ))
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('.') => {
                                    self.next();
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
                            self.next();
                            if let Some('<') = self.peek() {
                                self.next();
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
                            self.next();
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::HyperlinkFuse),
                            ))
                        }
                        Some('>') => {
                            self.next();
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
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            Ok(Token::new(
                                Span::new(start, self.cur_pos()),
                                TokenKind::Operator(Operator::FLt),
                            ))
                        }
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('.') => {
                                    self.next();
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
                            self.next();
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
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
                                    match self.peek() {
                                        Some('.') => {
                                            self.next();
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
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
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
                    self.next();
                    match self.peek() {
                        Some('=') => {
                            self.next();
                            match self.peek() {
                                Some('=') => {
                                    self.next();
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
                    self.next();
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
        LexingResult { tokens, errors }
    }
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

    test_number!("0".to_owned(), TokenKind::Int(0));
    test_number!("0.".to_owned(), TokenKind::Int(0));
    test_number!("0x1234".to_owned(), TokenKind::Int(0x1234));
    test_number!("1234".to_owned(), TokenKind::Int(1234));
    test_number!("1234.5678".to_owned(), TokenKind::Float(1234.5678));

    wrong!("0x");
}

#[test]
fn test_lexing() {
    let source = r#"
    a, b, c.
    hello(X) :- X = 1.
    test @@ 1234 : b,c.
    "#;

    let source = Source::from_string(source.to_owned());
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    assert_eq!(result.errors.len(), 1);
    assert_eq!(result.tokens.len(), 22);
}

#[test]
fn test_lexing_operator() {
    let source = Source::from_string("'a' + b *. c =:= d".to_owned());
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
