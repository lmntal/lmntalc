use std::io;

use tokio::net::TcpListener;
use tower_lsp_server::{ClientSocket, LspService, Server};

use super::backend::Backend;

pub async fn run_stdio() -> io::Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = build_service();
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

pub async fn run_tcp(port: u16) -> io::Result<()> {
    let listener = TcpListener::bind(format!("127.0.0.1:{port}")).await?;
    let (stream, _) = listener.accept().await?;
    let (read, write) = tokio::io::split(stream);
    let (service, socket) = build_service();
    Server::new(read, write, socket).serve(service).await;
    Ok(())
}

fn build_service() -> (LspService<Backend>, ClientSocket) {
    LspService::build(Backend::new).finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_service_for_bootstrap() {
        let _ = build_service();
    }
}
