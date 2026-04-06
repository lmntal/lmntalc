use lmntalc_ide::{
    AnalysisConfig, AnalysisDepth, AnalysisSession, OutlineKind, OutlineSymbol, Source,
};
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer};

use super::{
    capabilities,
    config::Config,
    diagnostics::{range_from_span, to_lsp_diagnostics},
    semantic_tokens::to_semantic_tokens,
};

pub struct Backend {
    client: Client,
    config: RwLock<Config>,
    session: RwLock<AnalysisSession>,
}

impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(capabilities::capabilities())
    }

    async fn initialized(&self, _: InitializedParams) {
        let config_items = self
            .client
            .configuration(vec![ConfigurationItem {
                scope_uri: None,
                section: Some("lmntal".to_string()),
            }])
            .await;

        let mut updated_config = false;
        let mut config = self.config.write().await;

        if let Ok(config_items) = config_items
            && let Some(des_config) = config_items.into_iter().next()
            && let Ok(new) = serde_json::from_value(des_config)
        {
            *config = new;
            updated_config = true;
        }

        if !updated_config {
            self.client
                .log_message(
                    MessageType::ERROR,
                    "Failed to retrieve configuration from client.",
                )
                .await;
        }

        self.client
            .log_message(
                MessageType::INFO,
                format!("LMNtal Language Server v{}", env!("CARGO_PKG_VERSION")),
            )
            .await;

        self.client
            .log_message(MessageType::INFO, "Checking for updates...".to_string())
            .await;

        if config.check_for_updates
            && let Some(new_version) = check_update().await
        {
            self.client
                .show_message(
                    MessageType::INFO,
                    format!(
                        "A new version of the LMNtal Language Server is available: v{}",
                        new_version
                    ),
                )
                .await;
        }
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_configuration(&self, params: DidChangeConfigurationParams) {
        if let Ok(new) = serde_json::from_value(params.settings) {
            let mut config = self.config.write().await;
            *config = new;
            self.client
                .log_message(
                    MessageType::INFO,
                    "Updated configuration from client.".to_string(),
                )
                .await;
        }
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(change) = params.content_changes.into_iter().next() else {
            return;
        };

        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: change.text,
            version: params.text_document.version,
            language_id: String::new(),
        })
        .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let _ = params;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.session.write().await.remove_document(uri.as_str());
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let session = self.session.read().await;
        let Some(snapshot) = session.snapshot(params.text_document.uri.as_str()) else {
            return Ok(None);
        };

        if snapshot.outline().is_empty() {
            Ok(None)
        } else {
            Ok(Some(DocumentSymbolResponse::Nested(
                snapshot.outline().iter().map(to_document_symbol).collect(),
            )))
        }
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {
        let session = self.session.read().await;
        let Some(snapshot) = session.snapshot(
            params
                .text_document_position_params
                .text_document
                .uri
                .as_str(),
        ) else {
            return Ok(None);
        };

        let Some(offset) = offset_at(
            snapshot.source(),
            params.text_document_position_params.position.line,
            params.text_document_position_params.position.character,
        ) else {
            return Ok(None);
        };

        let highlights = snapshot
            .highlights_at_offset(offset)
            .into_iter()
            .map(|span| DocumentHighlight {
                range: range_from_span(span),
                kind: None,
            })
            .collect::<Vec<_>>();

        if highlights.is_empty() {
            Ok(None)
        } else {
            Ok(Some(highlights))
        }
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let session = self.session.read().await;
        let uri = params.text_document_position.text_document.uri;
        let Some(snapshot) = session.snapshot(uri.as_str()) else {
            return Ok(None);
        };

        let Some(offset) = offset_at(
            snapshot.source(),
            params.text_document_position.position.line,
            params.text_document_position.position.character,
        ) else {
            return Ok(None);
        };

        let references = snapshot
            .references_at_offset(offset)
            .into_iter()
            .map(|span| Location {
                uri: uri.clone(),
                range: range_from_span(span),
            })
            .collect::<Vec<_>>();

        if references.is_empty() {
            Ok(None)
        } else {
            Ok(Some(references))
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let session = self.session.read().await;
        let Some(snapshot) = session.snapshot(params.text_document.uri.as_str()) else {
            return Ok(None);
        };

        if snapshot.semantic_spans().is_empty() {
            Ok(None)
        } else {
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: to_semantic_tokens(snapshot.semantic_spans()),
            })))
        }
    }
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            config: RwLock::new(Config::default()),
            session: RwLock::new(AnalysisSession::with_config(AnalysisConfig {
                depth: AnalysisDepth::Lowering,
            })),
        }
    }

    async fn on_change(&self, doc: TextDocumentItem) {
        let uri = doc.uri;
        let version = doc.version;
        let text = doc.text;

        if text.is_empty() {
            self.session.write().await.remove_document(uri.as_str());
            self.client.publish_diagnostics(uri, Vec::new(), None).await;
            return;
        }

        let diagnostics = {
            let mut session = self.session.write().await;
            session.set_document(uri.as_str(), version, text);
            let snapshot = session
                .snapshot(uri.as_str())
                .expect("snapshot should exist after set_document");
            to_lsp_diagnostics(snapshot.source(), &uri, &snapshot.diagnostics())
        };

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }
}

fn to_document_symbol(symbol: &OutlineSymbol) -> DocumentSymbol {
    #[allow(deprecated)]
    DocumentSymbol {
        name: symbol.name.clone(),
        detail: None,
        kind: match symbol.kind {
            OutlineKind::InitialProcess => SymbolKind::NAMESPACE,
            OutlineKind::Rule => SymbolKind::FUNCTION,
            OutlineKind::Membrane => SymbolKind::STRUCT,
        },
        tags: None,
        deprecated: None,
        range: range_from_span(symbol.span),
        selection_range: range_from_span(symbol.selection_span),
        children: if symbol.children.is_empty() {
            None
        } else {
            Some(symbol.children.iter().map(to_document_symbol).collect())
        },
    }
}

async fn check_update() -> Option<semver::Version> {
    let client = reqwest::ClientBuilder::new()
        .user_agent(concat!(
            env!("CARGO_PKG_NAME"),
            "/",
            env!("CARGO_PKG_VERSION")
        ))
        .timeout(std::time::Duration::from_secs(2))
        .build()
        .ok()?;

    let response = client
        .get("https://crates.io/api/v1/crates/lmntal-language-server")
        .send()
        .await
        .ok()?;

    response
        .json::<serde_json::Value>()
        .await
        .ok()
        .and_then(|json| {
            let version = json["crate"]["max_version"].as_str()?;
            let version = semver::Version::parse(version).ok()?;
            let current = semver::Version::parse(env!("CARGO_PKG_VERSION")).ok()?;
            (version > current).then_some(version)
        })
}

fn offset_at(source: &Source, line: u32, column: u32) -> Option<usize> {
    let target_line = line as usize;
    let target_column = column as usize;

    let mut current_line = 0usize;
    let mut current_column = 0usize;

    for (offset, ch) in source.source().chars().enumerate() {
        if current_line == target_line && current_column == target_column {
            return Some(offset);
        }

        if ch == '\n' {
            current_line += 1;
            current_column = 0;
        } else {
            current_column += 1;
        }
    }

    if current_line == target_line && current_column == target_column {
        Some(source.source().chars().count())
    } else {
        None
    }
}
