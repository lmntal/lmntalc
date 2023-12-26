use std::{io, path::PathBuf};

use clap::Parser;
use lmntalc::{
    analyzer::analyze,
    ast::tree,
    parsing::{self},
    report::Reporter,
    transform::transform_lmntal,
    util::SourceCode,
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

fn main() -> io::Result<()> {
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
    res.report(&code)?;

    let analysis_res = analyze(&res.ast);
    analysis_res.report(&code)?;

    if cli.dump_ast {
        let t = tree(&res.ast, "Root membrane".to_owned());
        match t {
            Ok(t) => println!("{}", t),
            Err(e) => println!("{}", e),
        }
    }

    Ok(())
}
