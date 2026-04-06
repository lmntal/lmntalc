use std::{io, path::Path};

use crate::{
    analysis::{analyze, SemanticAnalysisResult},
    codegen::{Emitter, IRSet},
    frontend::{lexing, parsing},
    target::{BackendError, Target},
    transform::TransformResult,
    util::Source,
};

#[derive(Default)]
pub struct CompileOptions {
    pub target: Option<Target>,
}

#[derive(Debug)]
pub struct Compilation {
    pub source: Source,
    pub lexing: lexing::LexingResult,
    pub parsing: Option<parsing::ParsingResult>,
    pub analysis: Option<SemanticAnalysisResult>,
    pub transform: Option<TransformResult>,
    pub ir: Option<IRSet>,
    pub code: Option<String>,
    pub backend_error: Option<BackendError>,
}

impl Compilation {
    pub fn ast(&self) -> Option<&crate::Membrane> {
        self.parsing.as_ref().map(|parsing| &parsing.root)
    }

    pub fn ir(&self) -> Option<&IRSet> {
        self.ir.as_ref()
    }

    pub fn has_errors(&self) -> bool {
        !self.lexing.errors.is_empty()
            || self
                .parsing
                .as_ref()
                .is_some_and(|parsing| !parsing.parsing_errors.is_empty())
            || self
                .analysis
                .as_ref()
                .is_some_and(|analysis| !analysis.errors.is_empty())
            || self
                .transform
                .as_ref()
                .is_some_and(|transform| !transform.errors.is_empty())
            || self.backend_error.is_some()
    }
}

pub fn compile_file(path: &Path, options: &CompileOptions) -> io::Result<Compilation> {
    let source = Source::from_file(path)?;
    Ok(compile_source(source, options))
}

pub fn compile_source(source: Source, options: &CompileOptions) -> Compilation {
    let lexer = lexing::Lexer::new(&source);
    let mut lexing = lexer.lex();

    if !lexing.errors.is_empty() {
        return Compilation {
            source,
            lexing,
            parsing: None,
            analysis: None,
            transform: None,
            ir: None,
            code: None,
            backend_error: None,
        };
    }

    let parsing = parsing::Parser::new().parse(std::mem::take(&mut lexing.tokens));
    if !parsing.parsing_errors.is_empty() {
        return Compilation {
            source,
            lexing,
            parsing: Some(parsing),
            analysis: None,
            transform: None,
            ir: None,
            code: None,
            backend_error: None,
        };
    }

    let analysis = analyze(&parsing.root);
    if !analysis.errors.is_empty() {
        return Compilation {
            source,
            lexing,
            parsing: Some(parsing),
            analysis: Some(analysis),
            transform: None,
            ir: None,
            code: None,
            backend_error: None,
        };
    }

    let transform = crate::transform::transform_lmntal(&parsing.root);
    if !transform.errors.is_empty() {
        return Compilation {
            source,
            lexing,
            parsing: Some(parsing),
            analysis: Some(analysis),
            transform: Some(transform),
            ir: None,
            code: None,
            backend_error: None,
        };
    }

    let mut emitter = Emitter::new();
    emitter.generate(&transform.program);
    let ir = emitter.finish();

    let (code, backend_error) = match options.target {
        Some(target) => match target.emit(&ir) {
            Ok(code) => (Some(code), None),
            Err(error) => (None, Some(error)),
        },
        None => (None, None),
    };

    Compilation {
        source,
        lexing,
        parsing: Some(parsing),
        analysis: Some(analysis),
        transform: Some(transform),
        ir: Some(ir),
        code,
        backend_error,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ir::LMNtalIR, transform::TransformError};

    fn compile_text(source: &str, target: Option<Target>) -> Compilation {
        compile_source(
            Source::from_string(source.to_string()),
            &CompileOptions { target },
        )
    }

    #[test]
    fn rejects_nested_rules_in_membranes() {
        let compilation = compile_text("{a(X,b,Y),c(X,Y). a,b,c :- e}.", None);
        let transform = compilation
            .transform
            .expect("transform result should exist");
        assert!(transform
            .errors
            .iter()
            .any(|error| matches!(error, TransformError::UnsupportedNestedRule { .. })));
    }

    #[test]
    fn rejects_rule_and_process_contexts_early() {
        let compilation = compile_text("b{@rule, $p[A, B | *K]} :- int(A) | c(A).", None);
        let transform = compilation
            .transform
            .expect("transform result should exist");
        assert!(transform
            .errors
            .iter()
            .any(|error| matches!(error, TransformError::UnsupportedRuleContext { .. })));
        assert!(transform
            .errors
            .iter()
            .any(|error| matches!(error, TransformError::UnsupportedProcessContext { .. })));
    }

    #[test]
    fn rejects_unsupported_guard_constraints() {
        let compilation = compile_text("a(X) :- ground(X) | b(X).", None);
        let transform = compilation
            .transform
            .expect("transform result should exist");
        assert!(transform
            .errors
            .iter()
            .any(|error| matches!(error, TransformError::UnsupportedGuardConstraint { .. })));
    }

    #[test]
    fn reports_unconstrained_links_from_transform_solver() {
        let compilation = compile_text("a(X) :- int(X) | b(Y).", None);
        let transform = compilation
            .transform
            .expect("transform result should exist");
        assert!(transform
            .errors
            .iter()
            .any(|error| matches!(error, TransformError::UnconstrainedLink { .. })));
    }

    #[test]
    fn renames_duplicate_rule_ir_symbols_and_keeps_explicit_init_ir() {
        let compilation = compile_text("name @@ a :- b. name @@ c :- d. a.", None);
        let ir = compilation.ir.expect("ir should exist");
        assert_eq!(ir.rules.len(), 2);
        assert_eq!(ir.rules[0].name, "name");
        assert_eq!(ir.rules[1].name, "name_1");
        assert!(!ir.init.body.is_empty());
    }

    #[test]
    fn solver_outputs_unify_ir_for_body_equalities() {
        let compilation = compile_text("name @@ a(X,Y) :- X = Y. a(1,2).", None);
        let ir = compilation.ir.expect("ir should exist");
        assert!(ir.rules[0].cases[0]
            .body
            .iter()
            .any(|ir| matches!(ir, LMNtalIR::Unify { .. })));
    }

    #[test]
    fn backend_smoke_tests_cover_guards_and_hyperlinks() {
        let source = "name @@ a(X,!H) :- int(X) | b(X,!H). a(1,!H).";
        for target in [Target::Cpp, Target::Java, Target::Python] {
            let compilation = compile_text(source, Some(target));
            assert!(
                !compilation.has_errors(),
                "target {target:?} should compile cleanly"
            );
            let code = compilation.code.expect("backend output should exist");
            match target {
                Target::Cpp => {
                    assert!(code.contains("int main()"));
                    assert!(code.contains("create_hyperlink"));
                    assert!(code.contains("is_int"));
                }
                Target::Java => {
                    assert!(code.contains("class Main"));
                    assert!(code.contains("createHyperlink"));
                    assert!(code.contains("isInt"));
                }
                Target::Python => {
                    assert!(code.contains("def main():"));
                    assert!(code.contains("create_hyperlink"));
                    assert!(code.contains("is_int"));
                }
            }
        }
    }
}
