use std::io;

use clap::Parser;
use lmntal_language_server::{run_stdio, run_tcp};

#[derive(Parser, Debug)]
#[command(name = "LMNtal Language Server")]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, help = "Port number to listen for incoming connections")]
    port: Option<u16>,
}

#[tokio::main]
async fn main() -> io::Result<()> {
    let args = Args::parse();
    env_logger::init();

    if let Some(port) = args.port {
        run_tcp(port).await
    } else {
        run_stdio().await
    }
}
