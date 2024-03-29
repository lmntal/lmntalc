use std::{cell::Cell, fmt::Display};

use crate::{
    frontend::{
        ast::AtomName,
        lexing::Lexer,
        token::{Operator, Token, TokenKind},
    },
    util::{OneOf, Pos, Source, Span},
    FunctorName, LinkBundle, RuleContext,
};

use super::{
    ast::{Atom, Hyperlink, Link, Membrane, Process, ProcessContext, ProcessList, Rule},
    token::Number,
};

/// A parser for LMNtal source code
#[derive(Debug, Default)]
pub struct Parser {
    warnings: Vec<ParseWarning>,
    tokens: Vec<Token>,
    pos: Cell<usize>,
}

#[derive(Debug)]
pub enum ParseErrorType {
    ExpectAnItem,
    UnexpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },
    UnexpectedEOF,
}

#[derive(Debug)]
pub struct ParseError {
    pub ty: ParseErrorType,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum ParseWarningType {
    MissingCommaBetweenProcesses,
    MissingPeriodAtTheEnd,
}

#[derive(Debug, Clone)]
pub struct ParseWarning {
    pub ty: ParseWarningType,
    pub span: Span,
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
            Self::ExpectAnItem => {
                write!(f, "Expect an item")
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

    fn skip_until<F>(&self, f: F)
    where
        F: Fn(&TokenKind) -> bool,
    {
        while !f(&self.peek().kind) {
            self.next();
        }
    }

    /// Advance the position by one
    fn advance(&self, n: usize) {
        self.pos.set(self.pos.get() + n);
    }

    /// Rewind the position by n
    fn rewind(&self, n: usize) {
        self.pos.set(self.pos.get() - n);
    }

    /// Start parsing the source code, the lexing is done in the process
    pub fn parse(mut self, tokens: Vec<Token>) -> ParsingResult {
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
            if !self.skip(&TokenKind::Period) {
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
        match self.look_ahead(0).kind {
            TokenKind::AtomName(ref name_) | TokenKind::LinkName(ref name_) => {
                if self.look_ahead(1).kind == TokenKind::AtAt {
                    name = name_.clone();
                    name_span = self.peek().span;
                    self.advance(2);
                }
            }
            _ => {}
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
        if self.peek().kind == TokenKind::Period {
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
            if self.peek().kind == TokenKind::Period {
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

    fn parse_rule(&mut self) -> ParseResult<Rule> {
        let mut name = String::new();
        let mut name_span = Span::dummy();
        match self.look_ahead(0).kind {
            TokenKind::AtomName(ref name_) | TokenKind::LinkName(ref name_) => {
                if self.look_ahead(1).kind == TokenKind::AtAt {
                    name = name_.clone();
                    name_span = self.peek().span;
                    self.advance(2);
                }
            }
            _ => {}
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
            return Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::ColonDash,
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            });
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
        if self.peek().kind == TokenKind::Period {
            return Ok(Rule {
                name: (name, name_span),
                head,
                propagation,
                guard: None,
                body: None,
                span: Span::new(low, self.look_back(1).span.high()),
            });
        }

        let guard_or_body = self.parse_process_list()?;
        if self.skip(&TokenKind::Vert) {
            if self.peek().kind == TokenKind::Period {
                return Ok(Rule {
                    name: (name, name_span),
                    head,
                    propagation,
                    guard: Some(guard_or_body),
                    body: None,
                    span: Span::new(low, self.look_back(1).span.high()),
                });
            }
            let body = self.parse_process_list()?;
            Ok(Rule {
                name: (name, name_span),
                head,
                propagation,
                guard: Some(guard_or_body),
                body: Some(body),
                span: Span::new(low, self.look_back(1).span.high()),
            })
        } else {
            Ok(Rule {
                name: (name, name_span),
                head,
                propagation,
                guard: None,
                body: Some(guard_or_body),
                span: Span::new(low, self.look_back(1).span.high()),
            })
        }
    }

    /// Parse a process list
    fn parse_process_list(&mut self) -> ParseResult<ProcessList> {
        let mut processes = vec![];
        let low = self.peek().span.low();
        let mut warnings = vec![];
        loop {
            processes.push(self.parse_process()?);
            let rollback = self.pos.get();
            if let TokenKind::Comma = self.peek().kind {
                self.advance(1);
                continue;
            } else if let Ok(any) = self.parse_rule_or_process_list() {
                match any {
                    OneOf::Left(_) => {
                        self.pos.set(rollback); // leave for the next parsing
                        warnings.push(ParseWarning {
                            ty: ParseWarningType::MissingPeriodAtTheEnd,
                            span: self.peek().span,
                        });
                    }
                    OneOf::Right(process_list) => {
                        warnings.push(ParseWarning {
                            ty: ParseWarningType::MissingCommaBetweenProcesses,
                            span: self.peek().span,
                        });
                        processes.extend(process_list.processes);
                    }
                }
                continue;
            } else {
                break;
            }
        }

        // commit the warnings
        self.warnings.extend(warnings);
        Ok(ProcessList {
            processes,
            span: Span::new(low, self.look_back(1).span.high()),
        })
    }

    /// Process := Atom | NEG Atom | Aggregate
    fn parse_process(&mut self) -> ParseResult<Process> {
        let token = self.peek().clone();
        match token.kind {
            TokenKind::Operator(Operator::Negative) => {
                self.advance(1);
                let expr = self.parse_atom()?;
                Ok(expr)
            }
            _ => {
                if let Ok(atom) = self.parse_atom() {
                    Ok(atom)
                } else if let Ok(aggregate) = self.parse_aggregate() {
                    Ok(aggregate.into())
                } else {
                    Err(ParseError {
                        ty: ParseErrorType::ExpectAnItem,
                        span: token.span,
                    })
                }
            }
        }
    }

    fn parse_aggregate(&mut self) -> ParseResult<Atom> {
        let token = self.peek();
        let start = token.span.low();

        let name = if let Some(name) = self.try_func_name() {
            self.advance(1);
            (
                AtomName::Functor(name),
                Span::new(start, self.peek().span.high()),
            )
        } else {
            return Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::AtomName("func name".to_owned()),
                    found: token.kind.clone(),
                },
                span: token.span,
            });
        };

        self.expect(&TokenKind::LeftParen)?;

        let mut processes = vec![];

        while let Ok(res) = self.parse_bundle() {
            processes.push(res.into());
            if self.skip(&TokenKind::Period) {
                continue;
            }
            break;
        }

        let token = self.expect(&TokenKind::RightParen)?;

        Ok(Atom {
            name,
            args: processes,
            span: Span::new(start, token.span.high()),
        })
    }

    /// TopAtom := Relation | AtomName ":" TopAtom
    fn parse_atom(&mut self) -> ParseResult<Process> {
        let cur = self.peek().clone();
        let next = self.look_ahead(1).clone();
        if let TokenKind::Colon = next.kind {
            let name = match &cur.kind {
                TokenKind::AtomName(name) => {
                    Some(AtomName::Functor(FunctorName::AtomName(name.clone())))
                }
                TokenKind::Number(Number::Decimal(n)) => Some(AtomName::Int(*n)),
                _ => None,
            };
            if let Some(name) = name {
                self.advance(2);
                let atom = self.parse_atom()?;
                let span = cur.span.merge(atom.span());
                let args = vec![
                    Process::Atom(Atom {
                        name: (name, cur.span),
                        args: vec![],
                        span: cur.span,
                    }),
                    atom,
                ];
                Ok(Process::Atom(Atom {
                    name: (
                        AtomName::Functor(FunctorName::AtomName(":".to_owned())),
                        next.span,
                    ),
                    args,
                    span,
                }))
            } else {
                self.parse_relation()
            }
        } else {
            self.parse_relation()
        }
    }

    /// Parse a relation expression
    fn parse_relation(&mut self) -> ParseResult<Process> {
        let lhs = self.parse_expr(0)?;
        if self.peek().kind.is_relational() {
            let op = self.peek().kind.operator().unwrap();
            let op_span = self.peek().span;
            let span = self.peek().span;
            self.advance(1);
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
            TokenKind::Operator(op) if op.is_prefix() => {
                let rhs = self.parse_expr(op.prefix_binding_power())?;
                Process::Atom(Atom {
                    name: (op.into(), span),
                    args: vec![rhs],
                    span,
                })
            }
            TokenKind::LeftParen => {
                self.advance(1);
                let expr = self.parse_expr(0)?;
                self.expect(&TokenKind::RightParen)?;
                expr
            }
            _ => self.parse_unit_atom()?,
        };

        loop {
            let op = self.peek().kind.clone();
            let span = self.peek().span;
            if !op.is_arithmetic() {
                break;
            }
            let op = op.operator().unwrap();
            let (left_bp, right_bp) = op.infix_binding_power();
            if left_bp < min_bp {
                break;
            }
            self.advance(1);
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
    fn parse_unit_atom(&mut self) -> ParseResult<Process> {
        if let TokenKind::LeftParen = self.peek().kind {
            let rollback = self.pos.get();
            self.advance(1);
            if let TokenKind::Operator(op) = self.peek().kind {
                self.advance(1);
                let atom = Atom {
                    name: (op.into(), self.peek().span),
                    args: vec![],
                    span: self.peek().span,
                };
                self.expect(&TokenKind::RightParen)?;
                return Ok(atom.into());
            } else if let Ok(rule) = self.parse_rule() {
                self.expect(&TokenKind::RightParen)?;
                return Ok(rule.into());
            }
            self.pos.set(rollback);
        }
        if let Ok(res) = self.parse_membrane() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_single_atom() {
            Ok(res.into())
        } else if let Ok(res) = self.parse_list() {
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
                ty: ParseErrorType::ExpectAnItem,
                span: self.peek().span,
            })
        }
    }

    /// Parse a membrane: {a(X,b,Y),c(X,Y). a,b,c :- e}
    fn parse_membrane(&mut self) -> ParseResult<Membrane> {
        let token = self.peek();
        let mut m_name = String::new();
        let mut m_span = Span::dummy();

        if let TokenKind::AtomName(name) = &token.kind {
            self.advance(1);
            m_name = name.clone();
            m_span = token.span;
        }

        if !self.skip(&TokenKind::LeftBrace) {
            if !m_name.is_empty() {
                self.rewind(1); // rewind the name for subsequent parsing since it may be an atom
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
            if self.skip(&TokenKind::Period) {
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
    fn parse_single_atom(&mut self) -> ParseResult<Atom> {
        let token = self.peek();
        let span = token.span;
        match &token.kind {
            TokenKind::Char(c) => {
                self.advance(1);
                return Ok(Atom {
                    name: (AtomName::Char(*c), span),
                    args: Vec::new(),
                    span,
                });
            }
            TokenKind::Number(number) => match number {
                Number::Binary(n) | Number::Octal(n) | Number::Hexadecimal(n) => {
                    self.advance(1);
                    return Ok(Atom {
                        name: (AtomName::Int(*n), span),
                        args: Vec::new(),
                        span,
                    });
                }
                Number::Float(f) => {
                    self.advance(1);
                    return Ok(Atom {
                        name: (AtomName::Float(*f), span),
                        args: Vec::new(),
                        span,
                    });
                }
                _ => {}
            },
            _ => {}
        }

        let name = if let TokenKind::Number(Number::Decimal(d)) = token.kind {
            self.advance(1);
            AtomName::Int(d)
        } else {
            let func = self.try_func_name().ok_or_else(|| ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::AtomName("atom name".to_owned()),
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })?;
            AtomName::Functor(func)
        };
        self.advance(1);

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
        if let TokenKind::LinkName(ref ident) = self.peek().kind {
            self.advance(1);
            if self.skip(&TokenKind::Colon) {
                let attr = self.try_func_name();
                if attr.is_none() {
                    // try skip until the next comma or period
                    self.skip_until(|kind| kind == &TokenKind::Comma || kind == &TokenKind::Period);
                    Err(ParseError {
                        ty: ParseErrorType::UnexpectedToken {
                            expected: TokenKind::LinkName("funtor name".to_owned()),
                            found: self.peek().kind.clone(),
                        },
                        span: self.peek().span,
                    })
                } else {
                    self.advance(1); // skip the functor name
                    Ok(Hyperlink {
                        name: (ident.clone(), self.peek().span),
                        attr,
                        span: Span::new(start, self.peek().span.high()),
                    })
                }
            } else {
                Ok(Hyperlink {
                    name: (ident.clone(), self.peek().span),
                    attr: None,
                    span: Span::new(start, self.peek().span.high()),
                })
            }
        } else {
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::LinkName("link name".to_owned()),
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })
        }
    }

    fn parse_link(&mut self) -> ParseResult<Link> {
        let start = self.peek().span.low();
        let token = self.peek();
        if let TokenKind::LinkName(ref ident) = token.kind {
            self.advance(1);
            Ok(Link {
                name: ident.clone(),
                span: Span::new(start, token.span.high()),
            })
        } else {
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::LinkName("link name".to_owned()),
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
            TokenKind::AtomName(name) => {
                self.advance(1);
                name.clone()
            }
            _ => {
                return Err(ParseError {
                    ty: ParseErrorType::UnexpectedToken {
                        expected: TokenKind::AtomName("context name".to_owned()),
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
        if self.skip(&TokenKind::At) {
            let token = self.peek();
            if let TokenKind::AtomName(ref ident) = token.kind {
                self.advance(1);
                Ok(RuleContext {
                    name: (ident.clone(), token.span),
                    span: Span::new(start, token.span.high()),
                })
            } else {
                Err(ParseError {
                    ty: ParseErrorType::UnexpectedToken {
                        expected: TokenKind::AtomName("context name".to_owned()),
                        found: self.peek().kind.clone(),
                    },
                    span: self.peek().span,
                })
            }
        } else {
            Err(ParseError {
                ty: ParseErrorType::UnexpectedToken {
                    expected: TokenKind::At,
                    found: self.peek().kind.clone(),
                },
                span: self.peek().span,
            })
        }
    }

    /// Parse a process context: $Context
    fn parse_bundle(&mut self) -> ParseResult<LinkBundle> {
        let token = self.peek();
        let start = token.span.low();
        if token.kind == TokenKind::Operator(Operator::IMul) {
            self.advance(1);
            let token = self.peek();
            if let TokenKind::LinkName(ref ident) = token.kind {
                self.advance(1);
                Ok(LinkBundle {
                    name: (ident.clone(), token.span),
                    span: Span::new(start, token.span.high()),
                })
            } else {
                Err(ParseError {
                    ty: ParseErrorType::UnexpectedToken {
                        expected: TokenKind::LinkName("bundle name".to_owned()),
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

    /// [] -> "[]"
    /// [a] -> "."(a, "[]")
    /// [a, b, c] -> "."(a, "."(b, "."(c, "[]")))
    /// [a, b | c] -> "."(a, "."(b, c))
    fn parse_list(&mut self) -> ParseResult<Atom> {
        let start = self.peek().span.low();
        self.expect(&TokenKind::LeftBracket)?;

        if let Ok(token) = self.expect(&TokenKind::RightBracket) {
            return Ok(Atom {
                name: (
                    AtomName::new_plain("[]".to_owned()),
                    Span::new(start, token.span.high()),
                ),
                args: Vec::new(),
                span: self.peek().span,
            });
        }

        let mut args = vec![];
        let mut rem = false;

        while let Ok(process) = self.parse_unit_atom() {
            args.push(process);
            if self.skip(&TokenKind::Vert) {
                args.push(self.parse_unit_atom()?);
                rem = true;
                break; // no more arguments after the rem
            }
            // TODO: 1. missing comma
            // TODO: 2. maybe wrong period
            // Both of them should be reported as warnings
            if self.skip(&TokenKind::Comma) {
                continue;
            }
        }

        // concatenate the remaining list in reverse order
        let mut rev_args = args.into_iter().rev();
        let mut current = if rem {
            Atom {
                name: (AtomName::new_plain(".".to_owned()), Span::dummy()),
                args: vec![rev_args.next().unwrap(), rev_args.next().unwrap()],
                span: Span::dummy(),
            }
        } else {
            Atom {
                name: (AtomName::new_plain(".".to_owned()), Span::dummy()),
                args: vec![
                    rev_args.next().unwrap(),
                    Atom {
                        name: (AtomName::new_plain("[]".to_owned()), Span::dummy()),
                        args: vec![],
                        span: Span::dummy(),
                    }
                    .into(),
                ],
                span: Span::dummy(),
            }
        };

        for arg in rev_args {
            current = Atom {
                name: (AtomName::new_plain(".".to_owned()), Span::dummy()),
                args: vec![arg, current.into()],
                span: Span::dummy(),
            };
        }

        self.expect(&TokenKind::RightBracket)?;
        Ok(current)
    }

    /// Try to parse a functor name, without consuming the token (for immutable self).
    ///
    /// **Don't forget to advance the position if the parsing is successful**
    fn try_func_name(&self) -> Option<FunctorName> {
        let content = match &self.peek().kind {
            TokenKind::AtomName(s) => FunctorName::AtomName(s.clone()),
            TokenKind::PathedAtomName(s) => FunctorName::PathedAtomName(s.clone()),
            TokenKind::SymbolName(s) => FunctorName::SymbolName(s.replace("''", "'")),
            TokenKind::String(s) => FunctorName::String(s.clone()),
            TokenKind::Quoted(s) => FunctorName::QuotedString(s.clone()),
            _ => return None,
        };
        Some(content)
    }
}

/// Initialize the parser with a phony source code and parse the given function, used for testing
#[allow(dead_code)]
fn common_init<T>(source: &str, func: fn(&mut Parser) -> ParseResult<T>) -> ParseResult<T> {
    let source = Source::from_string(source.to_owned());
    let mut parser = Parser::new();
    let lexer = Lexer::new(&source);
    let result = lexer.lex();
    parser.tokens = result.tokens;
    func(&mut parser)
}

#[test]
fn test_parse_atom() {
    assert!(common_init("a(X,b,Y)", |p| p.parse_single_atom()).is_ok());
    assert!(common_init("(+)", |p| p.parse_unit_atom()).is_ok());
}

#[test]
fn test_parse_expr() {
    let result = common_init("a(X,b,Y) + b * c(e)", |p| p.parse_expr(0));
    assert!(result.is_ok());
}

#[test]
fn test_rule_context() {
    assert!(common_init("@name", |p| p.parse_rule_context()).is_ok());
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
    let lexer = Lexer::new(&source);
    let result = lexer.lex();
    let parser = Parser::new();
    let result = parser.parse(result.tokens);
    assert!(result.parsing_errors.is_empty());
}

#[test]
fn test_parse_membrane() {
    assert!(common_init("{a(X,b,Y),c(X,Y). a,b,c :- e}", |p| p.parse_membrane()).is_ok());
}

#[test]
fn test_parse_rule() {
    let res = common_init("b{@rule, $p[A, B | *K]} :- int(A) | c(X,Y)", |p| {
        p.parse_rule_or_process_list()
    });
    assert!(res.is_ok());
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
    // let source = Source::from_string("a(X,b,Y),c(X,Y). a,b \\ c :- 10.".to_owned());
    let source = Source::from_string("a,b , c :- 10.".to_owned());
    let lexer = Lexer::new(&source);
    let result = lexer.lex();
    let parser = Parser::new();
    let result = parser.parse(result.tokens);
    assert!(result.parsing_errors.is_empty());
}
