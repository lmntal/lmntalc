use crate::{lexing::Lexer, source_code::SourceCode, token::Token};

pub struct Parser<'lex> {
    source: &'lex SourceCode,
    tokens: Vec<Token>,
    pos: usize,
}

pub struct ParsingResult {}

impl<'lex> Parser<'lex> {
    pub fn new(source: &'lex SourceCode) -> Parser<'lex> {
        Parser {
            source,
            tokens: Vec::new(),
            pos: 0,
        }
    }

    pub fn parse(&mut self) -> ParsingResult {
        let mut lexer = Lexer::new(self.source);
        let result = lexer.lex();
        todo!()
    }
}
