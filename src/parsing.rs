use std::{cell::Cell, fmt::Display};

use crate::{
    ast::{ASTNode, AtomName},
    lexing::{LexError, Lexer},
    token::{Operator, Token, TokenKind},
    util::{SourceCode, Span},
};

/// A parser for LMNtal source code
#[derive(Debug)]
pub struct Parser<'lex> {
    source: &'lex SourceCode,
    warnings: Vec<ParseWarning>,
    tokens: Vec<Token>,
    pos: Cell<usize>,
}

#[derive(Debug)]
pub enum ParseErrorType {
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },
    UnexpectedEOF,
    /// Wrong case for an identifier, may be treated as a warning
    WrongCase(IdentifierKind),
}

#[derive(Debug)]
pub struct ParseError {
    pub ty: ParseErrorType,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum ParseWarningType {
    MissingCommaBetweenProcesses,
}

#[derive(Debug, Clone)]
pub struct ParseWarning {
    pub ty: ParseWarningType,
    pub span: Span,
}

#[derive(Debug)]
pub enum IdentifierKind {
    Atom,
    Membrane,
    Rule,
    Process,
    Link,
    Context,
}

impl IdentifierKind {
    pub fn should(&self) -> String {
        match self {
            IdentifierKind::Atom => "start with a lowercase letter",
            IdentifierKind::Membrane => "start with a lowercase letter",
            IdentifierKind::Rule => "start with a lowercase letter",
            IdentifierKind::Process => "start with a lowercase letter",
            IdentifierKind::Link => "start with an uppercase letter",
            IdentifierKind::Context => "start with a lowercase letter",
        }
        .to_owned()
    }
}

impl Display for IdentifierKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IdentifierKind::Atom => write!(f, "atom"),
            IdentifierKind::Membrane => write!(f, "membrane"),
            IdentifierKind::Rule => write!(f, "rule"),
            IdentifierKind::Process => write!(f, "process"),
            IdentifierKind::Link => write!(f, "link"),
            IdentifierKind::Context => write!(f, "context"),
        }
    }
}

impl Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected token: expected \'{}\', found \'{}\'",
                    expected, found
                )
            }
            Self::UnexpectedEOF => {
                write!(f, "Unexpected EOF")
            }
            Self::WrongCase(id) => {
                write!(f, "Wrong case for {}", id)
            }
        }
    }
}

type ParseResult = Result<ASTNode, ParseError>;

/// The result of parsing, containing the AST and the errors for reporting or recovery
#[derive(Debug)]
pub struct ParsingResult {
    pub ast: ASTNode,
    pub lexing_errors: Vec<LexError>,
    pub parsing_errors: Vec<ParseError>,
    pub parsing_warnings: Vec<ParseWarning>,
}

impl<'lex> Parser<'lex> {
    pub fn new(source: &'lex SourceCode) -> Parser<'lex> {
        Parser {
            source,
            warnings: Vec::new(),
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
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: kind.clone(),
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })
        }
    }

    /// Returns true if the parser has reached the end of the file
    fn eof(&self) -> bool {
        self.pos.get() >= self.tokens.len()
    }

    /// Returns true if the next token matches the given kind
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

    /// Advance the position by one
    fn advance(&self) {
        self.pos.set(self.pos.get() + 1);
    }

    /// Advance the position by n
    fn advance_n(&self, n: usize) {
        self.pos.set(self.pos.get() + n);
    }

    /// Rewind the position by one
    fn rewind(&self) {
        self.pos.set(self.pos.get() - 1);
    }

    /// Rewind the position by n
    fn rewind_n(&self, n: usize) {
        self.pos.set(self.pos.get() - n);
    }

    /// Start parsing the source code, the lexing is done in the process
    pub fn parse(&mut self) -> ParsingResult {
        let mut lexer = Lexer::new(self.source);
        let result = lexer.lex();
        self.tokens = result.tokens;

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
                process_lists: processes,
                rules,
                span: Span::new(0usize.into(), self.source.source().len().into()),
            },
            lexing_errors: result.errors,
            parsing_errors: errors,
            parsing_warnings: self.warnings.clone(),
        }
    }
}

impl<'lex> Parser<'lex> {
    /// Parse a rule or a process list
    ///
    /// Since a rule and a process list both start with an process list, it will be convenient to
    /// distinguish them by checking if the next token is a colon dash
    fn parse_rule_or_process_list(&mut self) -> ParseResult {
        let mut name = String::new();
        if self.look_ahead(0).kind == TokenKind::Identifier("".to_owned())
            && self.look_ahead(1).kind == TokenKind::AtAt
        {
            name = self.peek().pretty_print();
            self.advance_n(2);
        }
        let start = self.pos.get();
        let low = self.peek().span.low();
        let head = self.parse_process_list()?;

        let mut propagation = None;

        if self.skip(&TokenKind::Backslash) {
            propagation = Some(Box::new(self.parse_process_list()?));
        }

        if !self.skip(&TokenKind::ColonDash) {
            // ignore the backslash if it is not followed by a colon dash
            if let Some(propagation) = propagation {
                match (head, *propagation) {
                    (
                        ASTNode::ProcessList {
                            processes: mut p1,
                            span: span1,
                        },
                        ASTNode::ProcessList {
                            processes: mut p2,
                            span: span2,
                        },
                    ) => {
                        p1.append(&mut p2);
                        return Ok(ASTNode::ProcessList {
                            processes: p1,
                            span: span1.merge(span2),
                        });
                    }
                    _ => unreachable!(),
                }
            }
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

        // if the body is empty
        if self.peek().kind == TokenKind::Dot {
            return Ok(ASTNode::Rule {
                name,
                head: Box::new(head),
                propagation,
                guard: None,
                body: None,
                span: Span::new(low, self.look_back(1).span.high()),
            });
        }

        let guard_or_body = self.parse_process_list()?;
        if self.skip(&TokenKind::Vert) {
            if self.peek().kind == TokenKind::Dot {
                return Ok(ASTNode::Rule {
                    name,
                    head: Box::new(head),
                    propagation,
                    guard: Some(Box::new(guard_or_body)),
                    body: None,
                    span: Span::new(low, self.look_back(1).span.high()),
                });
            }
            let body = self.parse_process_list()?;
            Ok(ASTNode::Rule {
                name,
                head: Box::new(head),
                propagation,
                guard: Some(Box::new(guard_or_body)),
                body: Some(Box::new(body)),
                span: Span::new(low, self.look_back(1).span.high()),
            })
        } else {
            Ok(ASTNode::Rule {
                name,
                head: Box::new(head),
                propagation,
                guard: None,
                body: Some(Box::new(guard_or_body)),
                span: Span::new(low, self.look_back(1).span.high()),
            })
        }
    }

    /// Parse a process list
    fn parse_process_list(&mut self) -> ParseResult {
        let mut processes = vec![];
        let low = self.peek().span.low();
        let mut warnings = vec![];
        loop {
            processes.push(self.parse_relation()?);
            match self.peek().kind {
                TokenKind::Comma => {
                    self.advance();
                    continue;
                }
                // Missing a comma, treated as if it were present and give a warning
                TokenKind::Identifier(_) | TokenKind::LeftBrace => {
                    warnings.push(ParseWarning {
                        ty: ParseWarningType::MissingCommaBetweenProcesses,
                        span: self.peek().span,
                    });
                    continue;
                }
                _ => break,
            }
        }

        // commit the warnings
        self.warnings.extend(warnings);
        Ok(ASTNode::ProcessList {
            processes,
            span: Span::new(low, self.look_back(1).span.high()),
        })
    }

    fn parse_relation(&mut self) -> ParseResult {
        let lhs = self.parse_expr(0)?;
        if self.peek().kind.is_relational() {
            let op = self.peek().kind.operator().unwrap();
            let span = self.peek().span;
            self.advance();
            let rhs = self.parse_expr(0)?;
            Ok(ASTNode::Atom {
                name: op.into(),
                args: vec![lhs, rhs],
                span,
            })
        } else {
            Ok(lhs)
        }
    }

    /// Parse an expression, using Pratt's algorithm
    fn parse_expr(&mut self, min_bp: u8) -> ParseResult {
        let op = self.peek().kind.clone();
        let span = self.peek().span;
        let mut lhs = match op {
            TokenKind::Operator(op) if op == Operator::IAdd || op == Operator::ISub => {
                let rhs = self.parse_expr(prefix_binding_power(&op))?;
                ASTNode::Atom {
                    name: op.into(),
                    args: vec![rhs],
                    span,
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
            let span = self.peek().span;
            if !op.is_arithmetic() {
                break;
            }
            let op = op.operator().unwrap();
            let (left_bp, right_bp) = infix_binding_power(&op);
            if left_bp < min_bp {
                break;
            }
            self.advance();
            let rhs = self.parse_expr(right_bp)?;
            lhs = ASTNode::Atom {
                name: op.into(),
                args: vec![lhs, rhs],
                span,
            }
        }

        Ok(lhs)
    }

    /// Parse a process, which can be a membrane, an atom, a link or a context
    fn parse_process(&mut self) -> ParseResult {
        if let Ok(res) = self.parse_membrane() {
            Ok(res)
        } else if let Ok(res) = self.parse_atom() {
            Ok(res)
        } else if let Ok(res) = self.parse_link() {
            Ok(res)
        } else if let Ok(res) = self.parse_context() {
            Ok(res)
        } else {
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::Identifier("any identifier".to_owned()),
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })
        }
    }

    /// Parse a membrane: {a(X,b,Y),c(X,Y). a,b,c :- e}
    fn parse_membrane(&mut self) -> ParseResult {
        let token = self.peek();
        let span = token.span;
        let mut m_name = String::new();

        if let TokenKind::Identifier(name) = &token.kind {
            // check if the name starts with a non_uppercase letter
            if name.starts_with(|c: char| c.is_uppercase()) {
                return Err(ParseError {
                    ty: ParseErrorType::WrongCase(IdentifierKind::Membrane),
                    span: self.peek().span,
                });
            }
            self.advance();
            m_name = name.clone();
        }

        if !self.skip(&TokenKind::LeftBrace) {
            if !m_name.is_empty() {
                self.rewind(); // rewind the name for subsequent parsing since it may be an atom
            }

            return Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::LeftBrace,
                    found: token.kind.clone(),
                },
                span: self.peek().span,
            });
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
            process_lists: processes,
            span,
        })
    }

    /// Parse an atom: a(X,b,Y)
    fn parse_atom(&mut self) -> ParseResult {
        let token = self.peek();
        let span = token.span;
        let name = match &token.kind {
            TokenKind::Identifier(name) => {
                // check if the name starts with a non_uppercase letter
                if name.starts_with(|c: char| c.is_uppercase()) {
                    return Err(ParseError {
                        ty: ParseErrorType::WrongCase(IdentifierKind::Atom),
                        span: self.peek().span,
                    });
                }
                self.advance();
                AtomName::Plain(name.clone())
            }
            TokenKind::Keyword(key) => {
                self.advance();
                AtomName::Keyword(key.to_string())
            }
            TokenKind::Char(c) => {
                self.advance();
                AtomName::Char(*c)
            }
            TokenKind::Int(i) => {
                self.advance();
                AtomName::Int(*i)
            }
            TokenKind::Float(f) => {
                self.advance();
                AtomName::Float(*f)
            }
            _ => {
                return Err(ParseError {
                    ty: ParseErrorType::UnexpectedToken {
                        expected: TokenKind::Identifier("atom name".to_owned()),
                        found: self.peek().kind.clone(),
                    },
                    span: self.peek().span,
                });
            }
        };

        if !self.skip(&TokenKind::LeftParen) {
            Ok(ASTNode::Atom {
                name,
                args: Vec::new(),
                span,
            })
        } else {
            let mut children = vec![];

            loop {
                children.push(self.parse_relation()?);
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
                span,
            })
        }
    }

    fn parse_link(&mut self) -> ParseResult {
        let start = self.peek().span.low();
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
                    span: Span::new(start, token.span.high()),
                })
            } else {
                Err(ParseError {
                    ty: ParseErrorType::WrongCase(IdentifierKind::Link),
                    span: self.peek().span,
                })
            }
        } else {
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::Identifier("link name".to_owned()),
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })
        }
    }

    /// Parse a context: $Context
    fn parse_context(&mut self) -> ParseResult {
        let token = self.peek();
        let start = token.span.low();
        if let TokenKind::Dollar = token.kind {
            self.advance();
            let token = self.peek();
            if let TokenKind::Identifier(ref ident) = token.kind {
                if ident.starts_with(|c: char| c.is_lowercase()) {
                    self.advance();
                    Ok(ASTNode::Context {
                        name: ident.clone(),
                        span: Span::new(start, token.span.high()),
                    })
                } else {
                    Err(ParseError {
                        ty: ParseErrorType::WrongCase(IdentifierKind::Context),
                        span: self.peek().span,
                    })
                }
            } else {
                Err(ParseError {
                    ty: ParseErrorType::UnexpectedToken {
                        expected: TokenKind::Identifier("context name".to_owned()),
                        found: self.peek().kind.clone(),
                    },
                    span: self.peek().span,
                })
            }
        } else {
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::Dollar,
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })
        }
    }
}

/// Returns the binding power of the prefix operator
fn prefix_binding_power(op: &Operator) -> u8 {
    match op {
        Operator::ISub | Operator::IAdd | Operator::FAdd | Operator::FSub => 9,
        _ => unreachable!(),
    }
}

/// Returns the binding power of the infix operator
fn infix_binding_power(op: &Operator) -> (u8, u8) {
    use Operator::*;
    match op {
        IMul | IDiv | IMod | FMul | FDiv => (3, 4),
        IAdd | ISub | FAdd | FSub => (1, 2),
        _ => unreachable!(),
    }
}

/// Initialize the parser with a phony source code and parse the given function, used for testing
#[allow(dead_code)]
fn common_init(source: &str, func: fn(&mut Parser) -> ParseResult) -> ParseResult {
    let source = SourceCode::phony(source.to_owned());
    let mut parser = Parser::new(&source);
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    func(&mut parser)
}

#[test]
fn test_parse_atom() {
    assert!(common_init("a(X,b,Y)", |p| p.parse_atom()).is_ok());
}

#[test]
fn test_parse_expr() {
    let result = common_init("a(X,b,Y) + b * c(e)", |p| p.parse_expr(0));
    assert!(result.is_ok());
}

#[test]
fn test_parse_relation() {
    assert!(common_init("a <. Z", |p| p.parse_relation()).is_ok());
    assert!(common_init("a >< Z", |p| p.parse_relation()).is_ok());
    assert!(common_init("a << Z", |p| p.parse_relation()).is_ok());
    assert!(common_init("a =:= Z", |p| p.parse_relation()).is_ok());
}

#[test]
fn test_compicate_process() {
    let source = SourceCode::phony("a({X,b,Y + b} * c(e)) :- a({X + b :- k. a + c}).".to_owned());
    let mut parser = Parser::new(&source);
    let result = parser.parse();
    assert!(result.parsing_errors.is_empty());
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
    assert!(common_init("a(X,b,Y),c(X,Y)", |p| p.parse_rule_or_process_list()).is_ok());
    assert!(common_init("name @@ a(X,b,Y) :- int(A) | c(X,Y)", |p| p
        .parse_rule_or_process_list())
    .is_ok());
    assert!(common_init("a(X,b,Y) :- int(A) | c(X,Y)", |p| p
        .parse_rule_or_process_list())
    .is_ok());
}

#[test]
fn test_parse_world() {
    let source = SourceCode::phony("a(X,b,Y),c(X,Y). a,b \\ c :- 10.".to_owned());
    let mut parser = Parser::new(&source);
    let result = parser.parse();
    assert!(result.parsing_errors.is_empty());
}
