use lmntalc_ide::{SemanticKind, SemanticSpan};
use tower_lsp_server::ls_types::{SemanticToken, SemanticTokenType};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::NAMESPACE,
    SemanticTokenType::CLASS,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRUCT,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::OPERATOR,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
];

pub fn to_semantic_tokens(spans: &[SemanticSpan]) -> Vec<SemanticToken> {
    let mut spans = spans.to_vec();
    spans.sort_by_key(|span| (span.span.low().line, span.span.low().column));

    let mut last_line = 0;
    let mut last_start = 0;
    spans
        .into_iter()
        .map(|span| {
            let line = span.span.low().line;
            let col = span.span.low().column;
            let delta_line = line - last_line;
            let delta_start = if delta_line == 0 {
                col - last_start
            } else {
                col
            };
            last_line = line;
            last_start = col;

            SemanticToken {
                delta_line,
                delta_start,
                length: span.span.len() as u32,
                token_type: token_type(span.kind),
                token_modifiers_bitset: 0,
            }
        })
        .collect()
}

fn token_type(kind: SemanticKind) -> u32 {
    match kind {
        SemanticKind::Rule => 0,
        SemanticKind::Membrane => 1,
        SemanticKind::Atom => 2,
        SemanticKind::Link => 3,
        SemanticKind::Hyperlink => 4,
        SemanticKind::Context => 5,
        SemanticKind::KeywordAtom => 6,
        SemanticKind::OperatorAtom => 7,
        SemanticKind::StringAtom => 8,
        SemanticKind::NumberAtom => 9,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lmntalc_ide::{Pos, Span};

    #[test]
    fn encodes_delta_sorted_semantic_tokens() {
        let spans = vec![
            SemanticSpan {
                span: Span::new(Pos::new(4, 1, 1), Pos::new(5, 1, 2)),
                kind: SemanticKind::Link,
            },
            SemanticSpan {
                span: Span::new(Pos::new(0, 0, 0), Pos::new(4, 0, 4)),
                kind: SemanticKind::Rule,
            },
        ];

        let tokens = to_semantic_tokens(&spans);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].delta_line, 0);
        assert_eq!(tokens[0].delta_start, 0);
        assert_eq!(tokens[0].token_type, 0);
        assert_eq!(tokens[1].delta_line, 1);
        assert_eq!(tokens[1].delta_start, 1);
        assert_eq!(tokens[1].token_type, 3);
    }
}
