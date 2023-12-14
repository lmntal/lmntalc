pub mod guard;
pub mod rule;

use std::{
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::transform::ProcessHolder;

use self::rule::Rule;

#[derive(Debug, Clone, Copy, Eq)]
pub enum Process {
    Atom(usize),
    Membrane(usize),
    Link(usize),
    Hyperlink(usize),
}

#[derive(Debug, Default)]
pub struct Membrane {
    /// The ID of the parent membrane.
    pub parent: usize,
    pub name: String,
    pub processes: Vec<Process>,
    pub rules: Vec<Rule>,

    atom_store: Vec<Atom>,
    membrane_store: Vec<Membrane>,
    link_store: Vec<Link>,
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

#[derive(Debug, Default)]
pub struct Atom {
    pub parent: usize,
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

#[derive(Debug)]
pub struct Context {
    pub parent: usize,
    pub name: String,
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
    pub fn get_id(&self) -> usize {
        match self {
            Self::Atom(id) => *id,
            Self::Membrane(id) => *id,
            Self::Link(id) => *id,
            Self::Hyperlink(id) => *id,
        }
    }

    pub fn with_id(&self, id: usize) -> Self {
        match self {
            Self::Atom(_) => Self::Atom(id),
            Self::Membrane(_) => Self::Membrane(id),
            Self::Link(_) => Self::Link(id),
            Self::Hyperlink(_) => Self::Hyperlink(id),
        }
    }
}

impl Ord for Process {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self {
            Process::Atom(id) => match other {
                Process::Atom(other_id) => id.cmp(other_id),
                Process::Membrane(_) => std::cmp::Ordering::Greater,
                Process::Link(_) => std::cmp::Ordering::Greater,
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Membrane(id) => match other {
                Process::Atom(_) => std::cmp::Ordering::Less,
                Process::Membrane(other_id) => id.cmp(other_id),
                Process::Link(_) => std::cmp::Ordering::Greater,
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Link(id) => match other {
                Process::Atom(_) => std::cmp::Ordering::Less,
                Process::Membrane(_) => std::cmp::Ordering::Less,
                Process::Link(other_id) => id.cmp(other_id),
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Hyperlink(_) => unimplemented!(),
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

impl PartialOrd for Process {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Membrane {
    pub fn new(name: String) -> Membrane {
        Membrane {
            name,
            ..Default::default()
        }
    }

    pub fn add_processes(&mut self, processes: Vec<Process>) {
        self.processes.extend(processes);
    }

    /// Add a rule to the program
    /// Returns the index of the rule
    pub fn add_rule(&mut self, rule: Rule) -> usize {
        self.rules.push(rule);
        self.rules.len() - 1
    }

    fn solve_link(&mut self) {
        // preprocess
        self.processes.retain(|process| match process {
            Process::Atom(_) | Process::Membrane(_) => true,
            Process::Link(_) | Process::Hyperlink(_) => false,
        });
        // pass 1: make connections between links and atoms
        for (atom_id, atom) in self.atom_store.iter().enumerate() {
            for arg in &atom.args {
                if let Process::Link(id) = arg {
                    let link = &mut self.link_store[*id];
                    if link.arg1.is_none() {
                        link.arg1 = Some(Process::Atom(atom_id));
                    } else if link.arg2.is_none() {
                        link.arg2 = Some(Process::Atom(atom_id));
                    } else {
                        panic!("link {} has more than 2 args", link.name);
                    }
                }
            }
        }

        // TODO: alpha conversion

        // check free links
        for link in &self.link_store {
            if link.arg1.is_none() || link.arg2.is_none() {
                panic!("free link {}", link.name);
            }
        }

        // replace links with atoms
        for (atom_id, atom) in self.atom_store.iter_mut().enumerate() {
            for arg in atom.args.iter_mut() {
                if let Process::Link(id) = arg {
                    let link = &mut self.link_store[*id];
                    if let Some(Process::Atom(id)) = &link.arg1 {
                        if id == &atom_id {
                            *arg = link.arg2.unwrap();
                        } else {
                            *arg = link.arg1.unwrap();
                        }
                    }
                }
            }
        }
    }

    /// Resolve all the references in the program
    pub(crate) fn solve(&mut self) {
        self.solve_link();
        self.solve_rules();
    }

    fn solve_rules(&mut self) {
        for rule in &mut self.rules {
            rule.solve();
        }
    }
}

impl ProcessHolder for Membrane {
    fn add_atom(&mut self, atom: Atom) -> usize {
        self.atom_store.push(atom);
        self.atom_store.len() - 1
    }

    fn add_link(&mut self, link: Link) -> usize {
        for (i, l) in self.link_store.iter_mut().enumerate() {
            if l.name == link.name {
                l.arg2 = None;
                return i;
            }
        }
        self.link_store.push(link);
        self.link_store.len() - 1
    }

    fn add_membrane(&mut self, membrane: Membrane) -> usize {
        self.membrane_store.push(membrane);
        self.membrane_store.len() - 1
    }

    fn alpha_connect(&mut self, process: Process, other: Process) {
        match process {
            Process::Atom(id) => match other {
                Process::Atom(other_id) => {
                    self.atom_store[id].args.push(other);
                    self.atom_store[other_id].args.push(process);
                }
                Process::Membrane(_) => {
                    unimplemented!()
                }
                Process::Link(_) => {
                    self.atom_store[id].args.push(other);
                }
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Membrane(_) => unimplemented!(),
            Process::Link(_) => match other {
                Process::Atom(other_id) => {
                    self.atom_store[other_id].args.push(process);
                }
                Process::Membrane(_) => unimplemented!(),
                Process::Link(_) => {
                    unimplemented!("Alpha conversion between links is not supported yet!")
                }
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Hyperlink(_) => unimplemented!(),
        }
    }

    fn get_atom_mut(&mut self, id: usize) -> &mut Atom {
        &mut self.atom_store[id]
    }

    fn get_membrane_mut(&mut self, id: usize) -> &mut Membrane {
        &mut self.membrane_store[id]
    }

    fn get_atom(&self, id: usize) -> &Atom {
        &self.atom_store[id]
    }

    fn get_membrane(&self, id: usize) -> &Membrane {
        &self.membrane_store[id]
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
