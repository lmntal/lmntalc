use std::{io, path::Path};

use lmntalc_core::{
    codegen::{Emitter, IRSet},
    diagnostics::{Diagnostic, DiagnosticSeverity},
    lowering::{self, TransformResult},
    optimization::Optimizer,
    semantics::{SemanticAnalysisResult, analyze},
    syntax::{lexing, parsing},
    text::Source,
};

use crate::target::{BackendError, Target};

#[derive(Default)]
pub struct CompileOptions {
    pub target: Option<Target>,
}

#[derive(Debug)]
pub struct Compilation {
    source: Source,
    lexing: lexing::LexingResult,
    parsing: Option<parsing::ParsingResult>,
    analysis: Option<SemanticAnalysisResult>,
    transform: Option<TransformResult>,
    ir: Option<IRSet>,
    code: Option<String>,
    backend_error: Option<BackendError>,
}

impl Compilation {
    pub fn source(&self) -> &Source {
        &self.source
    }

    pub fn lexing(&self) -> &lexing::LexingResult {
        &self.lexing
    }

    pub fn parsing(&self) -> Option<&parsing::ParsingResult> {
        self.parsing.as_ref()
    }

    pub fn semantics(&self) -> Option<&SemanticAnalysisResult> {
        self.analysis.as_ref()
    }

    pub fn lowering(&self) -> Option<&TransformResult> {
        self.transform.as_ref()
    }

    pub fn ast(&self) -> Option<&lmntalc_core::syntax::ast::Membrane> {
        self.parsing.as_ref().map(|parsing| &parsing.root)
    }

    pub fn ir(&self) -> Option<&IRSet> {
        self.ir.as_ref()
    }

    pub fn code(&self) -> Option<&str> {
        self.code.as_deref()
    }

    pub fn backend_error(&self) -> Option<&BackendError> {
        self.backend_error.as_ref()
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics()
            .iter()
            .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error)
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        let mut diagnostics = self.lexing.diagnostics();
        if let Some(parsing) = &self.parsing {
            diagnostics.extend(parsing.diagnostics());
        }
        if let Some(analysis) = &self.analysis {
            diagnostics.extend(analysis.diagnostics());
        }
        if let Some(transform) = &self.transform {
            diagnostics.extend(transform.diagnostics());
        }
        if let Some(error) = &self.backend_error {
            diagnostics.push(error.diagnostic());
        }
        diagnostics
    }
}

pub fn compile_file(path: &Path, options: &CompileOptions) -> io::Result<Compilation> {
    let source = Source::from_file(path)?;
    Ok(compile_source(source, options))
}

pub fn compile_file_with_optimizer(
    path: &Path,
    options: &CompileOptions,
    optimizer: &mut Optimizer,
) -> io::Result<Compilation> {
    let source = Source::from_file(path)?;
    Ok(compile_source_with_optimizer(source, options, optimizer))
}

pub fn compile_source(source: Source, options: &CompileOptions) -> Compilation {
    let mut optimizer = Optimizer::default_pipeline();
    compile_source_with_optimizer(source, options, &mut optimizer)
}

pub fn compile_source_with_optimizer(
    source: Source,
    options: &CompileOptions,
    optimizer: &mut Optimizer,
) -> Compilation {
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

    let transform = lowering::transform_lmntal(&parsing.root);
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
    let mut ir = emitter.finish();
    optimizer.optimize(&mut ir);

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
    use lmntalc_core::diagnostics::DiagnosticStage;
    use lmntalc_core::lowering::TransformError;
    use lmntalc_core::optimization::{OptimizationPass, Optimizer};

    use crate::ir::LMNtalIR;

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
            .lowering()
            .expect("transform result should exist");
        assert!(
            transform
                .errors
                .iter()
                .any(|error| matches!(error, TransformError::UnsupportedNestedRule { .. }))
        );
    }

    #[test]
    fn rejects_rule_and_process_contexts_early() {
        let compilation = compile_text("b{@rule, $p[A, B | *K]} :- int(A) | c(A).", None);
        let transform = compilation
            .lowering()
            .expect("transform result should exist");
        assert!(
            transform
                .errors
                .iter()
                .any(|error| matches!(error, TransformError::UnsupportedRuleContext { .. }))
        );
        assert!(
            transform
                .errors
                .iter()
                .any(|error| matches!(error, TransformError::UnsupportedProcessContext { .. }))
        );
    }

    #[test]
    fn rejects_unsupported_guard_constraints() {
        let compilation = compile_text("a(X) :- ground(X) | b(X).", None);
        let transform = compilation
            .lowering()
            .expect("transform result should exist");
        assert!(
            transform
                .errors
                .iter()
                .any(|error| matches!(error, TransformError::UnsupportedGuardConstraint { .. }))
        );
    }

    #[test]
    fn reports_unconstrained_links_from_transform_solver() {
        let compilation = compile_text("a(X) :- int(X) | b(Y).", None);
        let transform = compilation
            .lowering()
            .expect("transform result should exist");
        assert!(
            transform
                .errors
                .iter()
                .any(|error| matches!(error, TransformError::UnconstrainedLink { .. }))
        );
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
        assert!(
            ir.rules[0].cases[0]
                .body
                .iter()
                .any(|ir| matches!(ir, LMNtalIR::Unify { .. }))
        );
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
            let code = compilation.code().expect("backend output should exist");
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

    #[test]
    fn diagnostics_include_lexing_stage_information() {
        let compilation = compile_text("\"unterminated", None);
        let diagnostics = compilation.diagnostics();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.stage == DiagnosticStage::Lexing
                && diagnostic.primary_span.is_some()
                && !diagnostic.message.is_empty()
        }));
    }

    #[test]
    fn diagnostics_include_parsing_stage_information() {
        let compilation = compile_text("a :-", None);
        let diagnostics = compilation.diagnostics();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.stage == DiagnosticStage::Parsing
                && diagnostic.primary_span.is_some()
                && !diagnostic.message.is_empty()
        }));
    }

    #[test]
    fn diagnostics_include_semantic_stage_information() {
        let compilation = compile_text("a(X).", None);
        let diagnostics = compilation.diagnostics();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.stage == DiagnosticStage::Semantics
                && diagnostic.primary_span.is_some()
                && diagnostic.message.contains("Free link X is not allowed")
        }));
    }

    #[test]
    fn diagnostics_include_lowering_stage_information() {
        let compilation = compile_text("a(X) :- ground(X) | b(X).", None);
        let diagnostics = compilation.diagnostics();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.stage == DiagnosticStage::Lowering
                && diagnostic.primary_span.is_some()
                && diagnostic
                    .message
                    .contains("Constraint ground is not supported by code generation yet")
        }));
    }

    #[derive(Default)]
    struct RenameRulePass;

    impl OptimizationPass for RenameRulePass {
        fn name(&self) -> &'static str {
            "rename-rule"
        }

        fn optimize(&mut self, ir: &mut IRSet) {
            if let Some(rule) = ir.rules.first_mut() {
                rule.name = "optimized_rule".to_string();
            }
        }
    }

    #[test]
    fn custom_optimizer_runs_before_backend_emission() {
        let source = Source::from_string("name @@ a :- b. a.".to_string());
        let mut optimizer = Optimizer::new();
        optimizer.add_pass(RenameRulePass);

        let compilation = compile_source_with_optimizer(
            source,
            &CompileOptions {
                target: Some(Target::Python),
            },
            &mut optimizer,
        );

        let ir = compilation.ir().expect("ir should exist");
        assert_eq!(ir.rules[0].name, "optimized_rule");
        let code = compilation.code().expect("backend output should exist");
        assert!(code.contains("optimized_rule"));
    }
}
