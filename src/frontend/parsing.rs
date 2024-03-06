use std::{cell::Cell, fmt::Display};

use crate::{
    frontend::{
        ast::AtomName,
        lexing::Lexer,
        token::{Operator, Token, TokenKind},
    },
    util::{OneOf, Pos, Source, Span},
    Bundle, RuleContext,
};

use super::ast::{Atom, Hyperlink, Link, Membrane, Process, ProcessContext, ProcessList, Rule};

/// A parser for LMNtal source code
#[derive(Debug, Default)]
pub struct Parser {
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
    Bundle,
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
            IdentifierKind::Bundle => "start with an uppercase letter",
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
            IdentifierKind::Bundle => write!(f, "bundle"),
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

type ParseResult<T> = Result<T, ParseError>;

/// The result of parsing, containing the AST and the errors for reporting or recovery
#[derive(Debug)]
pub struct ParsingResult {
    pub root: Membrane,
    pub parsing_errors: Vec<ParseError>,
    pub parsing_warnings: Vec<ParseWarning>,
}

#[allow(dead_code)]
impl Parser {
    pub fn new() -> Parser {
        Self::default()
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
    pub fn parse(&mut self, tokens: Vec<Token>) -> ParsingResult {
        self.tokens = tokens;
        self.pos.set(0);
        self.warnings.clear();

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
                    OneOf::Left(rule) => rules.push(rule),
                    OneOf::Right(process_list) => processes.push(process_list),
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
            root: Membrane {
                name: ("_init".to_owned(), Span::dummy()),
                process_lists: processes,
                rules,
                span: Span::new(Pos::default(), self.tokens.last().unwrap().span.high()),
            },
            parsing_errors: errors,
            parsing_warnings: self.warnings.clone(),
        }
    }
}

impl Parser {
    /// Parse a rule or a process list
    ///
    /// Since a rule and a process list both start with an process list, it will be convenient to
    /// distinguish them by checking if the next token is a colon dash
    fn parse_rule_or_process_list(&mut self) -> ParseResult<OneOf<Rule, ProcessList>> {
        let mut name = String::new();
        let mut name_span = Span::dummy();
        if self.look_ahead(0).kind == TokenKind::Identifier("".to_owned())
            && self.look_ahead(1).kind == TokenKind::AtAt
        {
            name = self.peek().pretty_print();
            name_span = self.peek().span;
            self.advance_n(2);
        }
        let start = self.pos.get();
        let low = self.peek().span.low();
        let mut head = self.parse_process_list()?;

        let mut propagation = None;

        if self.skip(&TokenKind::Backslash) {
            propagation = Some(head);
            head = self.parse_process_list()?;
        }

        if !self.skip(&TokenKind::ColonDash) {
            // ignore the backslash if it is not followed by a colon dash
            if let Some(mut propagation) = propagation {
                head.processes.append(&mut propagation.processes);
            }
            return Ok(OneOf::Right(head));
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
            return Ok(OneOf::Left(Rule {
                name: (name, name_span),
                head,
                propagation,
                guard: None,
                body: None,
                span: Span::new(low, self.look_back(1).span.high()),
            }));
        }

        let guard_or_body = self.parse_process_list()?;
        if self.skip(&TokenKind::Vert) {
            if self.peek().kind == TokenKind::Dot {
                return Ok(OneOf::Left(Rule {
                    name: (name, name_span),
                    head,
                    propagation,
                    guard: Some(guard_or_body),
                    body: None,
                    span: Span::new(low, self.look_back(1).span.high()),
                }));
            }
            let body = self.parse_process_list()?;
            Ok(OneOf::Left(Rule {
                name: (name, name_span),
                head,
                propagation,
                guard: Some(guard_or_body),
                body: Some(body),
                span: Span::new(low, self.look_back(1).span.high()),
            }))
        } else {
            Ok(OneOf::Left(Rule {
                name: (name, name_span),
                head,
                propagation,
                guard: None,
                body: Some(guard_or_body),
                span: Span::new(low, self.look_back(1).span.high()),
            }))
        }
    }

    /// Parse a process list
    fn parse_process_list(&mut self) -> ParseResult<ProcessList> {
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
        Ok(ProcessList {
            processes,
            span: Span::new(low, self.look_back(1).span.high()),
        })
    }

    fn parse_relation(&mut self) -> ParseResult<Process> {
        let lhs = self.parse_expr(0)?;
        if self.peek().kind.is_relational() {
            let op = self.peek().kind.operator().unwrap();
            let op_span = self.peek().span;
            let span = self.peek().span;
            self.advance();
            let rhs = self.parse_expr(0)?;
            Ok(Process::Atom(Atom {
                name: (op.into(), op_span),
                args: vec![lhs, rhs],
                span,
            }))
        } else {
            Ok(lhs)
        }
    }

    /// Parse an expression, using Pratt's algorithm
    fn parse_expr(&mut self, min_bp: u8) -> ParseResult<Process> {
        let op = self.peek().kind.clone();
        let span = self.peek().span;
        let mut lhs = match op {
            TokenKind::Operator(op) if op == Operator::IAdd || op == Operator::ISub => {
                let rhs = self.parse_expr(prefix_binding_power(&op))?;
                Process::Atom(Atom {
                    name: (op.into(), span),
                    args: vec![rhs],
                    span,
                })
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
            lhs = Process::Atom(Atom {
                name: (op.into(), span),
                args: vec![lhs, rhs],
                span,
            })
        }

        Ok(lhs)
    }

    /// Parse a process, which can be a membrane, an atom, a link or a context
    fn parse_process(&mut self) -> ParseResult<Process> {
        if let Ok(res) = self.parse_membrane() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_atom() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_hyperlink() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_link() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_process_context() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_rule_context() {
            Ok(res.into())
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
    fn parse_membrane(&mut self) -> ParseResult<Membrane> {
        let token = self.peek();
        let mut m_name = String::new();
        let mut m_span = Span::dummy();

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
            m_span = token.span;
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
                OneOf::Left(rule) => {
                    rules.push(rule);
                }
                OneOf::Right(process_list) => {
                    processes.push(process_list);
                }
            }
            if self.skip(&TokenKind::Dot) {
                continue;
            }
            break;
        }

        let token = self.expect(&TokenKind::RightBrace)?;

        Ok(Membrane {
            name: (m_name, m_span),
            rules,
            process_lists: processes,
            span: m_span.merge(token.span),
        })
    }

    /// Parse an atom: a(X,b,Y)
    fn parse_atom(&mut self) -> ParseResult<Atom> {
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
            Ok(Atom {
                name: (name, span),
                args: Vec::new(),
                span,
            })
        } else {
            let mut children = vec![];

            loop {
                children.push(self.parse_relation()?);
                if !self.skip(&TokenKind::Comma) {
                    break;
                }
            }

            let token = self.expect(&TokenKind::RightParen)?;
            Ok(Atom {
                name: (name, span),
                args: children,
                span: span.merge(token.span),
            })
        }
    }

    fn parse_hyperlink(&mut self) -> ParseResult<Hyperlink> {
        let start = self.peek().span.low();
        if !self.skip(&TokenKind::Bang) {
            return Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::Bang,
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            });
        }
        self.parse_link().map(|link| Hyperlink {
            name: (link.name, link.span),
            span: Span::new(start, link.span.high()),
        })
    }

    fn parse_link(&mut self) -> ParseResult<Link> {
        let start = self.peek().span.low();
        let token = self.peek();
        if let TokenKind::Identifier(ref ident) = token.kind {
            if ident.starts_with(|c: char| c.is_uppercase()) {
                self.advance();
                Ok(Link {
                    name: ident.clone(),
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

    /// Parse a process context: $context
    fn parse_process_context(&mut self) -> ParseResult<ProcessContext> {
        let start = self.peek().span.low();
        if !self.skip(&TokenKind::Dollar) {
            return Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::Dollar,
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            });
        }
        let token = self.peek();
        let name = match &token.kind {
            TokenKind::Identifier(name) => {
                // check if the name starts with a non_uppercase letter
                if name.starts_with(|c: char| c.is_uppercase()) {
                    return Err(ParseError {
                        ty: ParseErrorType::WrongCase(IdentifierKind::Context),
                        span: self.peek().span,
                    });
                }
                self.advance();
                name.clone()
            }
            _ => {
                return Err(ParseError {
                    ty: ParseErrorType::UnexpectedToken {
                        expected: TokenKind::Identifier("context name".to_owned()),
                        found: self.peek().kind.clone(),
                    },
                    span: self.peek().span,
                });
            }
        };
        let name_span = token.span;

        if !self.skip(&TokenKind::LeftBracket) {
            return Ok(ProcessContext {
                name: (name, name_span),
                args: Vec::new(),
                bundle: None,
                span: Span::new(start, name_span.high()),
            });
        }

        let mut args = vec![];
        loop {
            args.push(self.parse_link()?);
            if !self.skip(&TokenKind::Comma) {
                break;
            }
        }

        if self.skip(&TokenKind::RightBracket) {
            return Ok(ProcessContext {
                name: (name, name_span),
                args,
                bundle: None,
                span: Span::new(start, name_span.high()),
            });
        }

        // TODO: Error handling when unclosed
        if !self.skip(&TokenKind::Vert) {
            return Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::Vert,
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            });
        }

        let bundle = self.parse_bundle().map(Some)?;
        let token = self.expect(&TokenKind::RightBracket)?;

        Ok(ProcessContext {
            name: (name, name_span),
            args,
            bundle,
            span: Span::new(start, token.span.high()),
        })
    }

    /// Parse a process context: @context
    fn parse_rule_context(&mut self) -> ParseResult<RuleContext> {
        let token = self.peek();
        let start = token.span.low();
        if !self.skip(&TokenKind::At) {
            let token = self.peek();
            if let TokenKind::Identifier(ref ident) = token.kind {
                if ident.starts_with(|c: char| c.is_lowercase()) {
                    self.advance();
                    Ok(RuleContext {
                        name: (ident.clone(), token.span),
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

    /// Parse a process context: $Context
    fn parse_bundle(&mut self) -> ParseResult<Bundle> {
        let token = self.peek();
        let start = token.span.low();
        if token.kind == TokenKind::Operator(Operator::IMul) {
            self.advance();
            let token = self.peek();
            if let TokenKind::Identifier(ref ident) = token.kind {
                if ident.starts_with(|c: char| c.is_uppercase()) {
                    self.advance();
                    Ok(Bundle {
                        name: (ident.clone(), token.span),
                        span: Span::new(start, token.span.high()),
                    })
                } else {
                    Err(ParseError {
                        ty: ParseErrorType::WrongCase(IdentifierKind::Bundle),
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
fn common_init<T>(source: &str, func: fn(&mut Parser) -> ParseResult<T>) -> ParseResult<T> {
    let source = Source::from_string(source.to_owned());
    let mut parser = Parser::new();
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
    let source = Source::from_string("a({X,b,Y + b} * c(e)) :- a({X + b :- k. a + c}).".to_owned());
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    let mut parser = Parser::new();
    let result = parser.parse(result.tokens);
    assert!(result.parsing_errors.is_empty());
}

#[test]
fn test_parse_membrane() {
    assert!(common_init("{a(X,b,!Y),c(X,Y). a,b,c :- e}", |p| p.parse_membrane()).is_ok());
}

#[test]
fn test_parse_rule() {
    assert!(common_init(
        "test@@ a(X,b,Y), b{@rule, $p[A, B | *K]} :- int(A) | c(X,Y)",
        |p| p.parse_rule_or_process_list()
    )
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
    let source = Source::from_string("a(X,b,Y),c(X,Y). a,b \\ c :- 10.".to_owned());
    let mut lexer = Lexer::new(&source);
    let result = lexer.lex();
    let mut parser = Parser::new();
    let result = parser.parse(result.tokens);
    assert!(result.parsing_errors.is_empty());
}
