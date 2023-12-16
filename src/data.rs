pub mod guard;
pub mod rule;

use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::transform::Storage;

use self::rule::Rule;

macro_rules! ids {
    (
        $(
            $name:ident
        ),*
    ) => {
        $(
            #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
            pub struct $name(uuid::Uuid);

            impl From<uuid::Uuid> for $name {
                fn from(id: uuid::Uuid) -> Self {
                    Self(id)
                }
            }

            impl From<$name> for uuid::Uuid {
                fn from(id: $name) -> Self {
                    id.0
                }
            }
        )*
    };
}

ids! {
    RuleId,
    MembraneId,
    AtomId,
    LinkId,
    HyperLinkId
}

#[derive(Debug, Default)]
pub struct Program {
    root: MembraneId,

    rules: HashMap<uuid::Uuid, Rule>,
    membranes: HashMap<uuid::Uuid, Membrane>,
    atoms: HashMap<uuid::Uuid, Atom>,
    links: HashMap<uuid::Uuid, Link>,
    hyperlinks: HashMap<uuid::Uuid, HyperLink>,
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
    Link(LinkId),
    Hyperlink(HyperLinkId),
}

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
    fn add_atom(&mut self, atom: Atom) -> AtomId {
        let id = uuid::Uuid::new_v4();
        self.atoms.insert(id, atom);
        AtomId(id)
    }

    fn add_membrane(&mut self, membrane: Membrane) -> MembraneId {
        let id = uuid::Uuid::new_v4();
        self.membranes.insert(id, membrane);
        MembraneId(id)
    }

    fn add_rule(&mut self, rule: Rule) -> RuleId {
        let id = uuid::Uuid::new_v4();
        self.rules.insert(id, rule);
        RuleId(id)
    }

    fn add_link(&mut self, link: Link) -> LinkId {
        let id = uuid::Uuid::new_v4();
        self.links.insert(id, link);
        LinkId(id)
    }

    fn add_hyperlink(&mut self, hyperlink: HyperLink) -> HyperLinkId {
        let id = uuid::Uuid::new_v4();
        self.hyperlinks.insert(id, hyperlink);
        HyperLinkId(id)
    }

    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom> {
        self.atoms.get_mut(&id.0)
    }

    fn get_membrane_mut(&mut self, id: MembraneId) -> Option<&mut Membrane> {
        self.membranes.get_mut(&id.0)
    }

    fn alpha_connect(&mut self, left: Process, right: Process) {
        match (left, right) {
            (Process::Atom(id), Process::Atom(other_id)) => {
                self.atoms.get_mut(&id.0).unwrap().args.push(right);
                self.atoms.get_mut(&other_id.0).unwrap().args.push(left);
            }
            (Process::Atom(atom), Process::Link(link)) => {
                self.atoms.get_mut(&atom.0).unwrap().args.push(right);
            }
            (Process::Link(link), Process::Atom(atom)) => {
                self.atoms.get_mut(&atom.0).unwrap().args.push(left);
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
    pub fn get_id(&self) -> uuid::Uuid {
        match self {
            Self::Atom(id) => id.0,
            Self::Membrane(id) => id.0,
            Self::Link(id) => id.0,
            Self::Hyperlink(id) => id.0,
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
