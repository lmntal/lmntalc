pub mod rule;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use owo_colors::OwoColorize;

use crate::{
    data::{Data, Program},
    ir::{self, LMNtalIR},
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
        let atoms = program.atoms(program.root());
        let hyperlinks = program.hyperlinks();
        let mut counter = 0;
        let mut queue = vec![];
        let mut atom_id_map = HashMap::new();

        for (atom_id, atom) in &atoms {
            self.create_atom(counter, &atom.name, atom.args.len(), atom.data.clone());
            atom_id_map.insert(atom_id, counter);
            counter += 1;
        }

        for (hl_id, hl) in hyperlinks {
            self.create_hyperlink(counter, &hl.name);
            atom_id_map.insert(hl_id, counter);
            counter += 1;
        }

        for (this_id, atom) in &atoms {
            for (this_port, arg) in atom.args.iter().enumerate() {
                if let Some((op, port)) = arg.opposite {
                    if hyperlinks.contains_key(&op) {
                        queue.push(self.hyperlink(
                            atom_id_map[this_id],
                            this_port,
                            atom_id_map[&op],
                        ));
                    } else {
                        queue.push(self.link(
                            atom_id_map[this_id],
                            this_port,
                            atom_id_map[&op],
                            port,
                        ));
                    }
                }
            }
        }

        let mut exists: HashSet<(_, _)> = HashSet::new();

        while let Some(link) = queue.pop() {
            if let LMNtalIR::Link { src, dst } = link {
                if exists.contains(&(src, dst)) {
                    continue;
                }
                exists.insert((dst, src)); // reverse link
                self.init.push(link);
            } else {
                self.init.push(link);
            }
        }
    }
}

/// Functions for generating single LMNtalIR
impl Generator {
    fn create_hyperlink(&mut self, id: usize, name: &str) {
        self.init.push(LMNtalIR::CreateHyperlink {
            id,
            name: name.to_string(),
        });
    }

    fn create_atom(&mut self, id: usize, name: &str, arity: usize, data: Data) {
        self.init.push(LMNtalIR::CreateAtom {
            id,
            name: name.to_string(),
            arity,
            data,
        });
    }

    fn link(&mut self, src: usize, src_port: usize, dst: usize, dst_port: usize) -> LMNtalIR {
        LMNtalIR::Link {
            src: ir::VarSource::Head(src, src_port),
            dst: ir::VarSource::Head(dst, dst_port),
        }
    }

    fn hyperlink(&mut self, atom: usize, atom_port: usize, hl: usize) -> LMNtalIR {
        LMNtalIR::LinkToHyperlink {
            atom: ir::VarSource::Head(atom, atom_port),
            hyperlink: ir::VarSource::Head(hl, 0),
        }
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
