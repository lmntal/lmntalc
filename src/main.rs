use std::{io, path::PathBuf};

use clap::Parser;
use lmntalc::{
    compiler::{CompileOptions, compile_file},
    report::Reporter,
    target::Target,
    tree_root,
};
use owo_colors::OwoColorize;

#[derive(Parser)]
#[command(name = "LMNtal Compiler")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(short, long, default_value = "cpp", help = "Target language")]
    pub target: Target,

    #[arg(short, long, help = "Output file name")]
    pub output: Option<PathBuf>,

    #[arg(
        long,
        value_name = "dump ast",
        default_value = "false",
        help = "Dump AST"
    )]
    dump_ast: bool,

    #[arg(
        long,
        value_name = "show ir",
        default_value = "false",
        help = "Show compiled IR"
    )]
    show_ir: bool,

    #[arg(
        long,
        value_name = "parse only",
        default_value = "false",
        help = "Parse and analyze only, do not generate actual code. (Show AST and IR is still available)"
    )]
    parse_only: bool,

    #[arg(value_name = "FILE")]
    source: PathBuf,
}

fn main() -> io::Result<()> {
    let cli = Cli::parse();
    let compilation = compile_file(
        &cli.source,
        &CompileOptions {
            target: (!cli.parse_only).then_some(cli.target),
        },
    )?;

    compilation.report(compilation.source())?;

    if cli.dump_ast
        && let Some(ast) = compilation.ast()
    {
        match tree_root(ast) {
            Ok(tree) => println!("{}\n{}", "AST:".bold().underline(), tree),
            Err(error) => println!("{}", error),
        }
    }

    if cli.show_ir
        && let Some(ir) = compilation.ir()
    {
        println!("{}\n{}", "Compiled IR:".bold().underline(), ir);
    }

    if compilation.has_errors() || cli.parse_only {
        return Ok(());
    }

    let output_file_name = cli.output.unwrap_or_else(|| {
        let mut file_name = cli.source.clone();
        file_name.set_extension(cli.target.extension());
        file_name
    });

    if let Some(code) = compilation.code() {
        std::fs::write(&output_file_name, code)?;
    }

    Ok(())
}
