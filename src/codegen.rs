pub mod rule;

use std::{collections::HashMap, fmt::Display};

use owo_colors::OwoColorize;

use crate::{ir::LMNtalIR, model::Program};

use self::rule::{RuleGenerator, RuleIR};

/// A generator for AST from LMNtal program
#[derive(Debug, Default)]
pub struct Emitter {
    ir_set: IRSet,
}

#[derive(Debug, Default)]
pub struct IRSet {
    pub init: Vec<LMNtalIR>,
    pub rules: Vec<RuleIR>,
}

impl Emitter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn ir_set(&self) -> &IRSet {
        &self.ir_set
    }

    /// Generate AST from LMNtal program
    ///
    /// The result is a vector of AST nodes in which each node represents a function
    pub fn generate(&mut self, program: &Program) {
        self.gen_rules(program);
        self.gen_init(program);
    }

    fn gen_rules(&mut self, program: &Program) {
        let mut names = HashMap::new();
        for rule in program.root_rules() {
            let mut rule_gen = RuleGenerator::new(rule);
            let mut ir = rule_gen.generate();
            if let Some(count) = names.get_mut(&ir.name) {
                ir.name = format!("{}_{}", ir.name, count);
                *count += 1;
            } else {
                names.insert(ir.name.clone(), 1);
            }
            self.ir_set.rules.push(ir);
        }
    }

    /// Generate initialization function, e.g. `main()` in most languages
    fn gen_init(&mut self, program: &Program) {
        let rule = program.init_rule();
        let mut rule_gen = RuleGenerator::new(rule);
        let ir = rule_gen.generate();
        self.ir_set.init = ir.init();
    }
}

impl Display for IRSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", "Init".bold().underline().red())?;
        for ir in &self.init {
            writeln!(f, "\t{}", ir)?;
        }
        writeln!(f, "{}", "Rules".bold().underline().red())?;
        for rule in &self.rules {
            writeln!(f, "{}", rule)?;
        }
        Ok(())
    }
}
