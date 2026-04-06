use tower_lsp_server::ls_types::{
    DocumentFilter, InitializeResult, OneOf, SemanticTokensFullOptions, SemanticTokensLegend,
    SemanticTokensOptions, SemanticTokensRegistrationOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, StaticRegistrationOptions, TextDocumentRegistrationOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    WorkDoneProgressOptions,
};

use super::semantic_tokens::LEGEND_TYPE;

pub fn capabilities() -> InitializeResult {
    let semantic_tokens_registration_options = SemanticTokensRegistrationOptions {
        text_document_registration_options: TextDocumentRegistrationOptions {
            document_selector: Some(vec![DocumentFilter {
                language: Some("lmntal".to_string()),
                scheme: None,
                pattern: None,
            }]),
        },
        semantic_tokens_options: SemanticTokensOptions {
            work_done_progress_options: WorkDoneProgressOptions::default(),
            legend: SemanticTokensLegend {
                token_types: LEGEND_TYPE.into(),
                token_modifiers: vec![],
            },
            range: Some(false),
            full: Some(SemanticTokensFullOptions::Bool(true)),
        },
        static_registration_options: StaticRegistrationOptions::default(),
    };

    InitializeResult {
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    ..Default::default()
                },
            )),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                    semantic_tokens_registration_options,
                ),
            ),
            references_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            document_highlight_provider: Some(OneOf::Left(true)),
            ..ServerCapabilities::default()
        },
        server_info: Some(ServerInfo {
            name: "LMNtal Language Server".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        }),
        offset_encoding: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_advertises_implemented_capabilities() {
        let capabilities = capabilities().capabilities;
        assert!(capabilities.semantic_tokens_provider.is_some());
        assert!(capabilities.references_provider.is_some());
        assert!(capabilities.document_symbol_provider.is_some());
        assert!(capabilities.document_highlight_provider.is_some());
        assert!(capabilities.code_action_provider.is_none());
        assert!(capabilities.definition_provider.is_none());
        assert!(capabilities.rename_provider.is_none());
        assert!(capabilities.document_formatting_provider.is_none());
        assert!(capabilities.hover_provider.is_none());
    }
}
