pub mod guard;
pub mod id;
pub mod rule;

use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::transform::Storage;

use self::rule::Rule;

use id::*;

#[derive(Debug, Default)]
pub struct Program {
    root: MembraneId,

    rules: HashMap<RuleId, Rule>,
    membranes: HashMap<MembraneId, Membrane>,
    atoms: HashMap<AtomId, Atom>,
    links: HashMap<LinkId, Link>,
    hyperlinks: HashMap<HyperLinkId, HyperLink>,

    id_generator: IdGenerator,
}

impl Program {
    pub(crate) fn set_root(&mut self, root: MembraneId) {
        self.root = root;
    }

    pub fn root(&self) -> MembraneId {
        self.root
    }

    pub fn atoms(&self) -> &HashMap<AtomId, Atom> {
        &self.atoms
    }

    pub fn atoms_mut(&mut self) -> &mut HashMap<AtomId, Atom> {
        &mut self.atoms
    }

    pub fn membranes(&self) -> &HashMap<MembraneId, Membrane> {
        &self.membranes
    }

    pub fn membranes_mut(&mut self) -> &mut HashMap<MembraneId, Membrane> {
        &mut self.membranes
    }

    pub fn links(&self) -> &HashMap<LinkId, Link> {
        &self.links
    }

    pub fn links_mut(&mut self) -> &mut HashMap<LinkId, Link> {
        &mut self.links
    }

    pub fn hyperlinks(&self) -> &HashMap<HyperLinkId, HyperLink> {
        &self.hyperlinks
    }

    pub fn hyperlinks_mut(&mut self) -> &mut HashMap<HyperLinkId, HyperLink> {
        &mut self.hyperlinks
    }

    pub fn rules(&self) -> &HashMap<RuleId, Rule> {
        &self.rules
    }

    pub fn rules_mut(&mut self) -> &mut HashMap<RuleId, Rule> {
        &mut self.rules
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
    /// A link in a membrane
    ///
    /// The first usize is the id of the parent membrane
    ///
    /// The second usize is the id of the link
    Link(LinkId),
    /// A hyperlink in a membrane
    ///
    /// The first usize is the id of the parent membrane
    ///
    /// The second usize is the id of the hyperlink
    Hyperlink(HyperLinkId),
}

#[allow(dead_code)]
#[derive(Debug, Default)]
pub struct Membrane {
    parent: MembraneId,

    name: String,
    rules: Vec<RuleId>,
    membranes: Vec<MembraneId>,
    atoms: Vec<AtomId>,
    links: Vec<LinkId>,
    hyperlinks: Vec<HyperLinkId>,
}

#[derive(Debug, Default)]
pub struct Atom {
    pub parent: MembraneId,
    pub name: String,
    pub data: Data,
    pub args: Vec<Process>,
}

#[derive(Debug)]
pub struct Link {
    pub name: String,
    pub arg1: Option<Process>,
    pub arg2: Option<Process>,
}

#[derive(Debug)]
pub struct HyperLink {
    pub name: String,
    pub args: Vec<Process>,
}

#[derive(Debug, Default, Clone)]
pub enum Data {
    #[default]
    Empty,
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
}

impl Storage for Program {
    fn add_atom(&mut self, atom: Atom, parent: MembraneId) -> AtomId {
        let id = self.id_generator.next_atom_id(parent);
        self.atoms.insert(id, atom);
        id
    }

    fn add_membrane(&mut self, membrane: Membrane, parent: MembraneId) -> MembraneId {
        let id = self.id_generator.next_membrane_id(parent);
        self.membranes.insert(id, membrane);
        id
    }

    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> RuleId {
        let id = self.id_generator.next_rule_id(parent);
        self.rules.insert(id, rule);
        id
    }

    fn add_link(&mut self, link: Link, parent: MembraneId) -> LinkId {
        let id = self.id_generator.next_link_id(parent);
        self.links.insert(id, link);
        id
    }

    fn add_hyperlink(&mut self, hyperlink: HyperLink, parent: MembraneId) -> HyperLinkId {
        let id = self.id_generator.next_hyperlink_id(parent);
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom> {
        self.atoms.get_mut(&id)
    }

    fn get_membrane_mut(&mut self, id: MembraneId) -> Option<&mut Membrane> {
        self.membranes.get_mut(&id)
    }

    fn alpha_connect(&mut self, left: Process, right: Process) {
        match (left, right) {
            (Process::Atom(id), Process::Atom(other_id)) => {
                self.atoms.get_mut(&id).unwrap().args.push(right);
                self.atoms.get_mut(&other_id).unwrap().args.push(left);
            }
            (Process::Atom(atom), Process::Link(_)) => {
                self.atoms.get_mut(&atom).unwrap().args.push(right);
            }
            (Process::Link(_), Process::Atom(atom)) => {
                self.atoms.get_mut(&atom).unwrap().args.push(left);
            }
            _ => unimplemented!(),
        }
    }
}

impl Membrane {
    pub(crate) fn add_processes(&mut self, processes: Vec<Process>) {
        for process in processes {
            match process {
                Process::Atom(id) => self.atoms.push(id),
                Process::Membrane(id) => self.membranes.push(id),
                Process::Link(id) => self.links.push(id),
                Process::Hyperlink(id) => self.hyperlinks.push(id),
            }
        }
    }

    pub(crate) fn add_rule(&mut self, rule: RuleId) {
        self.rules.push(rule);
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
            Self::Link(id) => {
                state.write_u8(2);
                id.hash(state);
            }
            Self::Hyperlink(id) => {
                state.write_u8(3);
                id.hash(state);
            }
        }
    }
}

impl Process {
    pub fn get_id(&self) -> u64 {
        match self {
            Self::Atom(id) => (*id).into(),
            Self::Membrane(id) => (*id).into(),
            Self::Link(id) => (*id).into(),
            Self::Hyperlink(id) => (*id).into(),
        }
    }
}

impl PartialEq for Process {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Atom(l0), Self::Atom(r0)) => l0 == r0,
            (Self::Membrane(l0), Self::Membrane(r0)) => l0 == r0,
            (Self::Link(l0), Self::Link(r0)) => l0 == r0,
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

impl Link {
    pub fn new(name: &str) -> Link {
        Link {
            name: name.to_string(),
            arg1: None,
            arg2: None,
        }
    }
}

impl HyperLink {
    pub fn new(name: &str) -> HyperLink {
        HyperLink {
            name: name.to_string(),
            args: Vec::new(),
        }
    }
}
