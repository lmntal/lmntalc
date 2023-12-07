use std::path::PathBuf;

use clap::Parser;
use lmntalc::{
    ast::tree,
    parsing::{self},
    source_code::SourceCode,
};

#[derive(Parser)]
#[command(name = "LMNtal Compiler")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(value_name = "FILE")]
    source: PathBuf,

    #[arg(long, value_name = "dump ast", default_value = "false")]
    dump_ast: bool,
}

fn main() {
    let cli = Cli::parse();
    let path = &cli.source;

    if !path.exists() {
        panic!("File not found: {}", path.display());
    }
    if !path.is_file() {
        panic!("Not a file: {}", path.display());
    }

    let code = SourceCode::new(&cli.source);
    let mut parser = parsing::Parser::new(&code);
    let res = parser.parse();

    for e in res.lexing_errors.iter() {
        println!("{}", e.to_string(&code));
    }

    for e in res.parsing_errors.iter() {
        println!("{}", e);
    }

    if cli.dump_ast {
        let t = tree(&res.ast);
        match t {
            Ok(t) => println!("{}", t),
            Err(e) => println!("{}", e),
        }
    }
}
