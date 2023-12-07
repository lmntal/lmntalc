use std::{cell::Cell, fmt::Display};

use crate::{
    ast::ASTNode,
    lexing::{LexError, Lexer},
    source_code::SourceCode,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'lex> {
    source: &'lex SourceCode,
    tokens: Vec<Token>,
    lex_errors: Vec<LexError>,
    pos: Cell<usize>,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken {
        expected: TokenKind,
        found: Token,
    },
    UnexpectedEOF,
    /// Wrong case for an identifier, may be treated as a warning
    WrongCase,
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
            ParseError::WrongCase => {
                write!(f, "Wrong case")
            }
            ParseError::Placeholder => todo!(),
        }
    }
}

pub type ParseResult = Result<ASTNode, ParseError>;

#[derive(Debug)]
pub struct ParsingResult {
    pub ast: ASTNode,
    pub errors: Vec<ParseError>,
}

impl<'lex> Parser<'lex> {
    pub fn new(source: &'lex SourceCode) -> Parser<'lex> {
        Parser {
            source,
            tokens: Vec::new(),
            lex_errors: Vec::new(),
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

    /// Start parsing the source code
    pub fn parse(&mut self) -> ParsingResult {
        let mut lexer = Lexer::new(self.source);
        let result = lexer.lex();
        self.tokens = result.tokens;
        self.lex_errors = result.errors;

        let mut processes = vec![];
        let mut rules = vec![];

        let mut errors = vec![];

        loop {
            if self.eof() {
                break;
            }
            let res = self.parse_rule_or_process_list();
            match res {
                Ok(ast) => match ast {
                    ASTNode::Rule { .. } => {
                        rules.push(ast);
                    }
                    ASTNode::ProcessList { .. } => {
                        processes.push(ast);
                    }
                    _ => unreachable!(),
                },
                Err(err) => {
                    errors.push(err);
                }
            }
            if !self.skip(&TokenKind::Dot) {
                break;
            }
        }

        ParsingResult {
            ast: ASTNode::Membrane {
                name: "_init".to_owned(),
                processes,
                rules,
            },
            errors,
        }
    }
}

impl<'lex> Parser<'lex> {
    fn parse_rule_or_process_list(&mut self) -> ParseResult {
        let mut name = String::new();
        if self.look_ahead(0).kind == TokenKind::Identifier("".to_owned())
            && self.look_ahead(1).kind == TokenKind::AtAt
        {
            name = self.peek().pretty_print();
            self.advance_n(2);
        }
        let start = self.pos.get();
        let head = self.parse_process_list()?;
        if !self.skip(&TokenKind::ColonDash) {
            return Ok(head);
        }
        let end = self.pos.get();
        if name.is_empty() {
            name = self.tokens[start..end - 1]
                .iter()
                .map(|t| t.pretty_print())
                .collect::<Vec<String>>()
                .join("");
        }
        let guard_or_body = self.parse_process_list()?;
        if self.skip(&TokenKind::Vert) {
            let body = self.parse_process_list()?;
            Ok(ASTNode::Rule {
                name,
                head: Box::new(head),
                guard: Some(Box::new(guard_or_body)),
                body: Box::new(body),
            })
        } else {
            Ok(ASTNode::Rule {
                name,
                head: Box::new(head),
                guard: None,
                body: Box::new(guard_or_body),
            })
        }
    }

    fn parse_process_list(&mut self) -> ParseResult {
        let mut processes = vec![];
        loop {
            processes.push(self.parse_expr(0)?);
            if self.skip(&TokenKind::Comma) {
                continue;
            } else {
                break;
            }
        }
        Ok(ASTNode::ProcessList { processes })
    }

    fn parse_expr(&mut self, min_bp: u8) -> ParseResult {
        let op = self.peek().kind.clone();
        let mut lhs = match op {
            TokenKind::IAdd | TokenKind::ISub => {
                let rhs = self.parse_expr(prefix_binding_power(&op))?;
                ASTNode::Atom {
                    name: op.to_string(),
                    args: vec![rhs],
                }
            }
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.parse_expr(0)?;
                self.expect(&TokenKind::RightParen)?;
                expr
            }
            _ => self.parse_process()?,
        };

        loop {
            let op = self.peek().kind.clone();
            if !op.is_operator() {
                break;
            }
            let (left_bp, right_bp) = infix_binding_power(&op);
            if left_bp < min_bp {
                break;
            }
            self.advance();
            let rhs = self.parse_expr(right_bp)?;
            lhs = ASTNode::Atom {
                name: op.to_string(),
                args: vec![lhs, rhs],
            }
        }

        Ok(lhs)
    }

    fn parse_process(&mut self) -> ParseResult {
        if let Ok(res) = self.parse_atom() {
            Ok(res)
        } else if let Ok(res) = self.parse_membrane() {
            Ok(res)
        } else if let Ok(res) = self.parse_link() {
            Ok(res)
        } else if let Ok(res) = self.parse_context() {
            Ok(res)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: TokenKind::Identifier("".to_owned()),
                found: self.peek().clone(),
            })
        }
    }

    fn parse_membrane(&mut self) -> ParseResult {
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

    fn parse_atom(&mut self) -> ParseResult {
        let token = self.peek();
        let name = match &token.kind {
            TokenKind::Identifier(name) | TokenKind::Keyword(name) => {
                // check if the name starts with a non_uppercase letter
                if name.starts_with(|c: char| c.is_uppercase()) {
                    return Err(ParseError::Placeholder);
                }
                self.advance();
                name.clone()
            }
            TokenKind::Char(c) => {
                self.advance();
                c.to_string()
            }
            TokenKind::Int(i) => {
                self.advance();
                i.to_string()
            }
            TokenKind::Float(f) => {
                self.advance();
                f.to_string()
            }
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: TokenKind::Identifier("".to_owned()),
                    found: token.clone(),
                });
            }
        };
        if !self.skip(&TokenKind::LeftParen) {
            Ok(ASTNode::Atom {
                name,
                args: Vec::new(),
            })
        } else {
            let mut children = vec![];

            loop {
                children.push(self.parse_expr(0)?);
                if self.skip(&TokenKind::Comma) {
                    continue;
                } else {
                    break;
                }
            }

            self.expect(&TokenKind::RightParen)?;
            Ok(ASTNode::Atom {
                name,
                args: children,
            })
        }
    }

    fn parse_link(&mut self) -> ParseResult {
        let hyperlink = if self.peek().kind == TokenKind::Bang {
            self.advance();
            true
        } else {
            false
        };
        let token = self.peek();
        // parse link
        if let TokenKind::Identifier(ref ident) = token.kind {
            if ident.starts_with(|c: char| c.is_uppercase()) {
                self.advance();
                Ok(ASTNode::Link {
                    name: ident.clone(),
                    hyperlink,
                })
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

    /// Parse a context: $Context
    fn parse_context(&mut self) -> ParseResult {
        let token = self.peek();
        if let TokenKind::Dollar = token.kind {
            self.advance();
            let token = self.peek();
            if let TokenKind::Identifier(ref ident) = token.kind {
                if ident.starts_with(|c: char| c.is_lowercase()) {
                    self.advance();
                    Ok(ASTNode::Context {
                        name: ident.clone(),
                    })
                } else {
                    Err(ParseError::WrongCase)
                }
            } else {
                Err(ParseError::UnexpectedToken {
                    expected: TokenKind::Identifier("".to_owned()),
                    found: token.clone(),
                })
            }
        } else {
            Err(ParseError::UnexpectedToken {
                expected: TokenKind::Dollar,
                found: token.clone(),
            })
        }
    }
}

fn prefix_binding_power(op: &TokenKind) -> u8 {
    match op {
        TokenKind::ISub | TokenKind::IAdd => 9,
        _ => unreachable!(),
    }
}

fn infix_binding_power(op: &TokenKind) -> (u8, u8) {
    match op {
        TokenKind::IAdd | TokenKind::ISub | TokenKind::FAdd | TokenKind::FSub => (3, 4),
        TokenKind::IMul | TokenKind::IDiv | TokenKind::IMod | TokenKind::FMul | TokenKind::FDiv => {
            (5, 6)
        }
        TokenKind::Equal => (2, 1), // assign need to be right associative and have lowest precedence
        _ => unreachable!(),
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

fn common_init(source: &str, func: fn(&mut Parser) -> ParseResult) -> ParseResult {
    let source = SourceCode::phony(source.to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    func(&mut parser)
}

#[test]
fn test_parse_expr() {
    let result = common_init("a(X,b,Y) + b * c(e)", |p| p.parse_expr(0));
    assert!(result.is_ok());
}

#[test]
fn test_compicate_process() {
    let source = SourceCode::phony("a({X,b,Y + b} * c(e)) :- a({X + b :- k. a + c}).".to_owned());
    let mut parser = Parser::new(&source);
    let result = parser.parse();
    assert!(result.errors.is_empty());
}

#[test]
fn test_parse_membrane() {
    assert!(common_init("{a(X,b,!Y),c(X,Y). a,b,c :- e}", |p| p.parse_membrane()).is_ok());
}

#[test]
fn test_parse_rule() {
    assert!(common_init("test@@ a(X,b,Y) :- int(A) | c(X,Y)", |p| p
        .parse_rule_or_process_list())
    .is_ok());
}

#[test]
fn test_parse_process_list() {
    assert!(common_init("a(X,b,Y),c(X,Y)", |p| p.parse_process_list()).is_ok());
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

#[test]
fn test_parse_world() {
    let source = SourceCode::phony("a(X,b,Y),c(X,Y). a,b,c :- 10.".to_owned());
    let mut parser = Parser::new(&source);
    let result = parser.parse();
    assert!(result.errors.is_empty());
}
