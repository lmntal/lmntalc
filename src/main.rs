use std::{
    io::{self, Write},
    path::PathBuf,
};

use clap::Parser;
use lmntalc::{
    analysis::analyze,
    codegen::{Emitter, IRSet},
    frontend::{
        ast::tree,
        parsing::{self},
    },
    report::Reporter,
    target::{cpp::CppBackend, java::JavaBackend, python::PythonBackend, Backend, Target},
    transform::transform_lmntal,
    util::Source,
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
    let path = &cli.source;

    if !path.exists() {
        panic!("File not found: {}", path.display());
    }
    if !path.is_file() {
        panic!("Not a file: {}", path.display());
    }

    let code = Source::new(&cli.source);
    let mut parser = parsing::Parser::new(&code);
    let res = parser.parse();
    res.report(&code)?;

    let analysis_res = analyze(&res.ast);
    analysis_res.report(&code)?;

    let transform_res = transform_lmntal(&res.ast);

    let mut gen = Emitter::new();
    gen.generate(&transform_res.program);
    let ir = gen.ir_set();

    if cli.dump_ast {
        let t = tree(&res.ast, "Root membrane".to_owned());
        match t {
            Ok(t) => println!("{}\n{}", "AST:".bold().underline(), t),
            Err(e) => println!("{}", e),
        }
    }

    if cli.show_ir {
        println!("{}\n{}", "Compiled IR:".bold().underline(), ir);
    }

    if cli.parse_only {
        return Ok(());
    }

    let output_file_name = if let Some(ref output_file_name) = cli.output {
        output_file_name.clone()
    } else {
        let mut file_name = path.clone();
        file_name.set_extension(cli.target.extension());
        file_name
    };

    let mut output_file = std::fs::File::create(output_file_name.clone())
        .unwrap_or_else(|_| panic!("cannot create file {}", output_file_name.display()));

    let code = match cli.target {
        Target::Cpp => {
            let mut backend = CppBackend::new();
            output(&mut backend, ir, &cli)
        }
        Target::Python => {
            let mut backend = PythonBackend::new();
            output(&mut backend, ir, &cli)
        }
        Target::Java => {
            let mut backend = JavaBackend::new();
            output(&mut backend, ir, &cli)
        }
    };

    output_file
        .write_all(code.as_bytes())
        .expect("cannot write file");

    Ok(())
}

fn output(backend: &mut impl Backend, ir_set: &IRSet, arg: &Cli) -> String {
    backend.pretty_print(ir_set)
}
