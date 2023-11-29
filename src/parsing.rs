use std::{cell::Cell, fmt::Display};

use crate::{
    lexing::Lexer,
    source_code::SourceCode,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'lex> {
    source: &'lex SourceCode,
    tokens: Vec<Token>,
    pos: Cell<usize>,
}

#[derive(Debug)]
pub enum ASTNode {
    Rule {
        head: Box<ASTNode>,
        guard: Option<Box<ASTNode>>,
        body: Box<ASTNode>,
    },
    ProcessList {
        processes: Vec<ASTNode>,
    },
    Membrane {
        name: String,
        processes: Vec<ASTNode>,
        rules: Vec<ASTNode>,
    },
    Atom {
        name: String,
        children: Vec<ASTNode>,
    },
    Link {
        name: String,
    },
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken { expected: TokenKind, found: Token },
    UnexpectedEOF,
    Placeholder,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected token: expected {}, found {}",
                    expected, found
                )
            }
            ParseError::UnexpectedEOF => {
                write!(f, "Unexpected EOF")
            }
            ParseError::Placeholder => todo!(),
        }
    }
}

pub type ParsingResult = Result<ASTNode, ParseError>;

impl<'lex> Parser<'lex> {
    pub fn new(source: &'lex SourceCode) -> Parser<'lex> {
        Parser {
            source,
            tokens: Vec::new(),
            pos: Cell::new(0),
        }
    }

    /// Returns the token at the current position and advances the position by one
    fn next(&self) -> &Token {
        self.pos.set(self.pos.get() + 1);
        self.look_back(1)
    }

    /// Returns the token at the current position
    fn peek(&self) -> &Token {
        self.look_ahead(0)
    }

    /// Returns the token at the current position + n
    fn look_ahead(&self, n: usize) -> &Token {
        static EOF: Token = Token::eof();
        if self.pos.get() + n >= self.tokens.len() {
            &EOF
        } else {
            &self.tokens[self.pos.get() + n]
        }
    }

    /// Returns the token at the current position - n
    fn look_back(&self, n: usize) -> &Token {
        static EOF: Token = Token::eof();
        if self.pos.get() < n {
            &EOF
        } else {
            &self.tokens[self.pos.get() - n]
        }
    }

    /// Returns the next token if it matches the given kind
    fn expect(&self, kind: &TokenKind) -> Result<&Token, ParseError> {
        if self.is_match(kind) {
            Ok(self.next())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: kind.clone(),
                found: self.peek().clone(),
            })
        }
    }

    fn eof(&self) -> bool {
        self.pos.get() >= self.tokens.len()
    }

    fn is_match(&self, kind: &TokenKind) -> bool {
        self.peek().kind == *kind
    }

    /// skip the next token if it matches the given kind
    ///
    /// returns true if the token was skipped
    /// returns false if the token was not skipped
    fn skip(&self, kind: &TokenKind) -> bool {
        if self.is_match(kind) {
            self.next();
            true
        } else {
            false
        }
    }

    fn advance(&self) {
        self.pos.set(self.pos.get() + 1);
    }

    fn advance_n(&self, n: usize) {
        self.pos.set(self.pos.get() + n);
    }

    fn rewind(&self) {
        self.pos.set(self.pos.get() - 1);
    }

    fn rewind_n(&self, n: usize) {
        self.pos.set(self.pos.get() - n);
    }

    pub fn parse(&mut self) -> ParsingResult {
        let mut lexer = Lexer::new(self.source);
        let result = lexer.lex();
        self.tokens = result.tokens;

        let mut processes = vec![];
        let mut rules = vec![];

        while let Ok(res) = self.parse_rule_or_process_list() {
            match res {
                ASTNode::Rule { .. } => {
                    rules.push(res);
                }
                ASTNode::ProcessList { .. } => {
                    processes.push(res);
                }
                _ => unreachable!(),
            }
            if self.skip(&TokenKind::Dot) {
                continue;
            } else {
                break;
            }
        }

        Ok(ASTNode::Membrane {
            name: "_init".to_owned(),
            rules,
            processes,
        })
    }
}

impl<'lex> Parser<'lex> {
    fn parse_rule_or_process_list(&mut self) -> ParsingResult {
        let head = self.parse_process_list()?;
        if self.expect(&TokenKind::ColonDash).is_err() {
            return Ok(head);
        }
        let guard_or_body = self.parse_process_list()?;
        if self.skip(&TokenKind::Vert) {
            let body = self.parse_process_list()?;
            Ok(ASTNode::Rule {
                head: Box::new(head),
                guard: Some(Box::new(guard_or_body)),
                body: Box::new(body),
            })
        } else {
            Ok(ASTNode::Rule {
                head: Box::new(head),
                guard: None,
                body: Box::new(guard_or_body),
            })
        }
    }

    fn parse_rule(&mut self) -> ParsingResult {
        let head = self.parse_process_list()?;
        self.expect(&TokenKind::ColonDash)?;
        let guard_or_body = self.parse_process_list()?;
        if self.skip(&TokenKind::Vert) {
            let body = self.parse_process_list()?;
            Ok(ASTNode::Rule {
                head: Box::new(head),
                guard: Some(Box::new(guard_or_body)),
                body: Box::new(body),
            })
        } else {
            Ok(ASTNode::Rule {
                head: Box::new(head),
                guard: None,
                body: Box::new(guard_or_body),
            })
        }
    }

    fn parse_process_list(&mut self) -> ParsingResult {
        let mut processes = vec![];
        loop {
            processes.push(self.parse_process()?);
            if self.skip(&TokenKind::Comma) {
                continue;
            } else {
                break;
            }
        }
        Ok(ASTNode::ProcessList { processes })
    }

    fn parse_process(&mut self) -> ParsingResult {
        if let Ok(res) = self.parse_atom() {
            Ok(res)
        } else if let Ok(res) = self.parse_membrane() {
            Ok(res)
        } else {
            let token = self.peek();
            // parse link
            if let TokenKind::Identifier(ref ident) = token.kind {
                if ident.starts_with(|c: char| c.is_uppercase()) {
                    self.advance();
                    return Ok(ASTNode::Link {
                        name: ident.clone(),
                    });
                } else {
                    Err(ParseError::UnexpectedToken {
                        expected: TokenKind::Identifier("".to_owned()),
                        found: token.clone(),
                    })
                }
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: TokenKind::Identifier("".to_owned()),
                    found: token.clone(),
                })
            }
        }
    }

    fn parse_membrane(&mut self) -> ParsingResult {
        let token = self.peek();
        let mut m_name = String::new();
        match &token.kind {
            // check if the membrane has a name
            TokenKind::Identifier(name) => {
                // check if the name starts with a non_uppercase letter
                if name.starts_with(|c: char| c.is_uppercase()) {
                    return Err(ParseError::Placeholder);
                }
                self.advance();
                m_name = name.clone();
            }
            // if the membrane has no name, we keep going
            TokenKind::LeftBrace => {
                self.advance();
            }
            // if the membrane has no name and no left brace, we failed to parse
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: TokenKind::Identifier("".to_owned()),
                    found: token.clone(),
                })
            }
        }

        let mut processes = vec![];
        let mut rules = vec![];

        while let Ok(res) = self.parse_rule_or_process_list() {
            match res {
                ASTNode::Rule { .. } => {
                    rules.push(res);
                }
                ASTNode::ProcessList { .. } => {
                    processes.push(res);
                }
                _ => unreachable!(),
            }
            if self.skip(&TokenKind::Dot) {
                continue;
            } else {
                break;
            }
        }

        self.expect(&TokenKind::RightBrace)?;
        Ok(ASTNode::Membrane {
            name: m_name,
            rules,
            processes,
        })
    }

    fn parse_atom(&mut self) -> ParsingResult {
        let token = self.peek();
        match &token.kind {
            TokenKind::Identifier(name) | TokenKind::Keyword(name) => {
                // check if the name starts with a non_uppercase letter
                if name.starts_with(|c: char| c.is_uppercase()) {
                    return Err(ParseError::Placeholder);
                }
                self.advance();
                if !self.skip(&TokenKind::LeftParen) {
                    Ok(ASTNode::Atom {
                        name: name.clone(),
                        children: Vec::new(),
                    })
                } else {
                    let mut children = vec![];
                    let name = name.clone();

                    loop {
                        children.push(self.parse_process()?);
                        if self.skip(&TokenKind::Comma) {
                            continue;
                        } else {
                            break;
                        }
                    }

                    self.expect(&TokenKind::RightParen)?;
                    Ok(ASTNode::Atom { name, children })
                }
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: TokenKind::Identifier("".to_owned()),
                found: token.clone(),
            }),
        }
    }
}

#[test]
fn test_parse_atom() {
    let source = SourceCode::phony("a(X,b,Y)".to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    let result = parser.parse_atom();
    assert!(result.is_ok());
}

#[test]
fn test_parse_membrane() {
    let source = SourceCode::phony("{a(X,b,Y),c(X,Y). a,b,c :- e}".to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    let result = parser.parse_membrane();
    assert!(result.is_ok());
}

#[test]
fn test_parse_rule() {
    let source = SourceCode::phony("a(X,b,Y) :- int(A) | c(X,Y)".to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    let result = parser.parse_rule();
    assert!(result.is_ok());
}

#[test]
fn test_parse_process_list() {
    let source = SourceCode::phony("a(X,b,Y),c(X,Y)".to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    let result = parser.parse_process_list();
    assert!(result.is_ok());
}

#[test]
fn test_parse_rule_or_process_list() {
    let source = SourceCode::phony("a(X,b,Y),c(X,Y)".to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    let result = parser.parse_rule_or_process_list();
    assert!(result.is_ok());

    let source = SourceCode::phony("a(X,b,Y) :- int(A) | c(X,Y)".to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    let result = parser.parse_rule_or_process_list();
    assert!(result.is_ok());
}
