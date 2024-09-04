mod event;
mod grammar;
mod input;
mod lexed_str;
mod output;
mod parser;
mod syntax_kind;
mod token_set;
mod util;

pub(crate) use token_set::TokenSet;

pub use crate::{
    grammar::*,
    input::Input,
    lexed_str::LexedStr,
    output::{Output, Step},
    parser::Parser,
    syntax_kind::SyntaxKind,
};

#[test]
fn parsing_test() {
    use event::process;
    let src = "a(b)";
    let input = LexedStr::new(src).to_input();
    let mut p = Parser::new(&input);
    grammar::world(&mut p);
    let output = p.finish();
    process(output);
}
