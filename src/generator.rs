pub mod rule;

use std::{collections::HashMap, fmt::Display};

use owo_colors::OwoColorize;

use crate::{
    data::{Data, Program},
    ir::LMNtalIR,
};

use self::rule::{RuleGenerator, RuleIR};

/// A generator for AST from LMNtal program
#[derive(Debug, Default)]
pub struct Generator {
    pub init: Vec<LMNtalIR>,
    pub rules: Vec<RuleIR>,
}

impl Generator {
    pub fn new() -> Self {
        Self::default()
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
        for rule in &program.rules(program.root()) {
            let mut rule_gen = RuleGenerator::new(rule);
            let mut ir = rule_gen.generate();
            if let Some(count) = names.get_mut(&ir.name) {
                ir.name = format!("{}_{}", ir.name, count);
                *count += 1;
            } else {
                names.insert(ir.name.clone(), 1);
            }
            self.rules.push(ir);
        }
    }

    /// Generate initialization function, e.g. `main()` in most languages
    fn gen_init(&mut self, program: &Program) {
        let atoms = &program.atoms(program.root());
        let mut adj = vec![vec![0; atoms.len()]; atoms.len()];
        let mut atom_id_map = HashMap::new();
        let mut id_atom_map = HashMap::new();

        for (i, (atom_id, _)) in atoms.iter().enumerate() {
            atom_id_map.insert(atom_id, i);
            id_atom_map.insert(i, atom_id);
        }

        for (i, (_, atom)) in atoms.iter().enumerate() {
            self.create_atom(i, &atom.name, atom.args.len(), atom.data.clone());

            for (j, arg) in atom.args.iter().enumerate() {
                if let Some(op) = arg.opposite {
                    adj[i][atom_id_map[&op.0]] = j + 1;
                }
            }
        }

        for i in 0..atoms.len() {
            for j in 0..=i {
                if adj[i][j] != 0 {
                    self.link(i, adj[i][j] - 1, j, adj[j][i] - 1);
                }
            }
        }
    }
}

/// Functions for generating single LMNtalIR
impl Generator {
    fn create_atom(&mut self, id: usize, name: &str, arity: usize, data: Data) {
        self.init.push(LMNtalIR::CreateAtom {
            id,
            name: name.to_string(),
            arity,
            data,
        });
    }

    fn link(&mut self, src: usize, src_port: usize, dst: usize, dst_port: usize) {
        self.init.push(LMNtalIR::Link {
            src: crate::ir::VarSource::Head(src, src_port),
            dst: crate::ir::VarSource::Head(dst, dst_port),
        });
    }
}

impl Display for Generator {
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
