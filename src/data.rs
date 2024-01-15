pub mod guard;
pub mod id;
pub mod rule;

use std::{
    collections::HashSet,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::transform::SolveResult;

use self::{guard::VariableId, rule::Rule};

use id::*;

#[derive(Debug, Default)]
pub struct Program {
    root_rules: Vec<Rule>,
    init_rule: Rule,
}

impl Program {
    pub(crate) fn set_init_rule(&mut self, rule: Rule) {
        self.init_rule = rule;
    }

    pub(crate) fn init_rule(&self) -> &Rule {
        &self.init_rule
    }

    pub(crate) fn root_rules(&self) -> &Vec<Rule> {
        &self.root_rules
    }

    pub(crate) fn add_rule(&mut self, rule: Rule) {
        self.root_rules.push(rule);
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub enum Process {
    /// An atom in a membrane
    ///
    /// The first usize is the id of the membrane
    ///
    /// The second usize is the id of the atom
    Atom(AtomId),
    /// A membrane in a membrane
    ///
    /// The first usize is the id of the parent membrane
    ///
    /// The second usize is the id of the membrane
    Membrane(MembraneId),
    /// A hyperlink in a membrane
    ///
    /// The first usize is the id of the parent membrane
    ///
    /// The second usize is the id of the hyperlink
    Hyperlink(HyperlinkId),
}

#[allow(dead_code)]
#[derive(Debug, Default, Clone)]
pub struct Membrane {
    parent: MembraneId,

    name: String,
    rules: Vec<RuleId>,
    membranes: HashSet<MembraneId>,
    atoms: HashSet<AtomId>,
}

#[derive(Debug, Default, Clone)]
pub struct Atom {
    pub parent: MembraneId,
    pub name: String,
    pub data: Data,
    pub hyperlink: bool,
    pub args: Vec<Link>,
}

#[derive(Debug, Default, Clone)]
pub struct Link {
    pub name: String,
    pub this: (AtomId, usize),
    pub opposite: Option<(AtomId, usize)>,
}

/// A hyperlink is treated as a special atom
#[derive(Debug, Default, Clone)]
pub struct Hyperlink {
    pub name: String,
    pub args: Vec<Link>,
}

#[derive(Debug, Default, Clone)]
pub enum Data {
    #[default]
    Empty,
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Variable(VariableId),
}

impl Data {
    pub fn is_empty(&self) -> bool {
        matches!(self, Data::Empty)
    }
}

impl Program {
    pub(crate) fn solve(&mut self) -> SolveResult {
        let mut res = SolveResult::default();

        res.combine(self.init_rule.solve());

        for rule in &mut self.root_rules {
            res.combine(rule.solve());
        }

        res
    }
}

impl Membrane {
    pub(crate) fn add_processes(&mut self, processes: Vec<Process>) {
        for process in processes {
            match process {
                Process::Atom(id) => {
                    self.atoms.insert(id);
                }
                Process::Membrane(id) => {
                    self.membranes.insert(id);
                }
                _ => {}
            }
        }
    }

    pub(crate) fn add_rule(&mut self, rule: RuleId) {
        self.rules.push(rule);
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self
            .args
            .iter()
            .map(|link| link.name.clone())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{}({})", self.name, args)
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Data::Empty => write!(f, "empty"),
            Data::Int(i) => write!(f, "{}", i),
            Data::Float(fl) => write!(f, "{}", fl),
            Data::Char(c) => write!(f, "\'{}\'", c),
            Data::String(s) => write!(f, "\"{}\"", s),
            Data::Variable(id) => write!(f, "{}", id),
        }
    }
}

impl Process {
    pub fn get_id(&self) -> u64 {
        match self {
            Self::Atom(id) => (*id).into(),
            Self::Membrane(id) => id.id().into(),
            Self::Hyperlink(id) => (*id).into(),
        }
    }
}

impl Hash for Process {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Atom(id) => {
                state.write_u8(0);
                id.hash(state);
            }
            Self::Membrane(id) => {
                state.write_u8(1);
                id.hash(state);
            }
            Self::Hyperlink(id) => {
                state.write_u8(3);
                id.hash(state);
            }
        }
    }
}

impl PartialEq for Process {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Atom(l0), Self::Atom(r0)) => l0 == r0,
            (Self::Membrane(l0), Self::Membrane(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Membrane {
    pub fn new(name: String, parent: MembraneId) -> Membrane {
        Membrane {
            parent,
            name,
            ..Default::default()
        }
    }
}
