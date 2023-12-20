pub mod guard;
pub mod id;
pub mod rule;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::transform::{SolveResult, Storage, TransformError};

use self::rule::Rule;

use id::*;

#[derive(Debug, Default)]
pub struct Program {
    root: MembraneId,

    membranes: HashMap<MembraneId, Membrane>,
    rules: HashMap<RuleId, Rule>,
    atoms: HashMap<AtomId, Atom>,
    links: HashMap<LinkId, Link>,
    hyperlinks: HashMap<HyperLinkId, HyperLink>,

    id_generator: id::IdGenerator,
}

impl Program {
    pub(crate) fn set_root(&mut self, root: MembraneId) {
        self.root = root;
    }

    pub fn root(&self) -> MembraneId {
        self.root
    }

    pub fn atoms(&self, membrane: MembraneId) -> Vec<Atom> {
        self.membranes.get(&membrane).map_or(vec![], |mem| {
            mem.atoms.iter().map(|id| self.atoms[id].clone()).collect()
        })
    }

    pub fn membranes(&self) -> &HashMap<MembraneId, Membrane> {
        &self.membranes
    }

    pub fn links(&self, membrane: MembraneId) -> Vec<Link> {
        self.membranes.get(&membrane).map_or(vec![], |mem| {
            mem.links.iter().map(|id| self.links[id].clone()).collect()
        })
    }

    pub fn hyperlinks(&self, membrane: MembraneId) -> Vec<HyperLink> {
        self.membranes.get(&membrane).map_or(vec![], |mem| {
            mem.hyperlinks
                .iter()
                .map(|id| self.hyperlinks[id].clone())
                .collect()
        })
    }

    pub fn rules(&self, membrane: MembraneId) -> Vec<Rule> {
        self.membranes.get(&membrane).map_or(vec![], |mem| {
            mem.rules.iter().map(|id| self.rules[id].clone()).collect()
        })
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
#[derive(Debug, Default, Clone)]
pub struct Membrane {
    parent: MembraneId,

    name: String,
    rules: Vec<RuleId>,
    membranes: HashSet<MembraneId>,
    atoms: HashSet<AtomId>,
    links: HashSet<LinkId>,
    hyperlinks: HashSet<HyperLinkId>,
}

#[derive(Debug, Default, Clone)]
pub struct Atom {
    pub parent: MembraneId,
    pub name: String,
    pub data: Data,
    pub args: Vec<Process>,
}

#[derive(Debug, Default, Clone)]
pub struct Link {
    pub name: String,
    pub arg1: Option<(Process, usize)>,
    pub arg2: Option<(Process, usize)>,
}

#[derive(Debug, Default, Clone)]
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
    fn next_membrane_id(&mut self) -> MembraneId {
        self.id_generator.next_membrane_id()
    }

    fn add_atom(&mut self, atom: Atom, parent: MembraneId) -> AtomId {
        let id = self.id_generator.next_atom_id(parent);
        self.atoms.insert(id, atom);
        id
    }

    fn add_membrane(&mut self, id: MembraneId, membrane: Membrane) {
        self.membranes.insert(id, membrane);
    }

    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> RuleId {
        let id = self.id_generator.next_rule_id(parent);
        self.rules.insert(id, rule);
        id
    }

    fn add_link(&mut self, link: Link, parent: MembraneId) -> LinkId {
        let links = self.link_with_id(parent);
        for (id, link_) in links.iter() {
            if link.name == link_.name {
                return *id;
            }
        }
        let id = self.id_generator.next_link_id(parent);
        self.links.insert(id, link);
        id
    }

    fn add_hyperlink(&mut self, hyperlink: HyperLink, parent: MembraneId) -> HyperLinkId {
        let hyperlinks = self.hyperlink_with_id(parent);
        for (id, hyperlink_) in hyperlinks.iter() {
            if hyperlink.name == hyperlink_.name {
                return *id;
            }
        }
        let id = self.id_generator.next_hyperlink_id(parent);
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    fn get_atom(&self, id: AtomId) -> Option<&Atom> {
        self.atoms.get(&id)
    }

    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom> {
        self.atoms.get_mut(&id)
    }

    fn alpha_connect(&mut self, left: Process, right: Process) {
        match (left, right) {
            (Process::Atom(id), Process::Atom(other_id)) => {
                self.get_atom_mut(id).unwrap().args.push(right);
                self.get_atom_mut(other_id).unwrap().args.push(left);
            }
            (Process::Atom(atom), Process::Link(_)) => {
                self.get_atom_mut(atom).unwrap().args.push(right);
            }
            (Process::Link(_), Process::Atom(atom)) => {
                self.get_atom_mut(atom).unwrap().args.push(left);
            }
            _ => unimplemented!(),
        }
    }
}

impl Program {
    fn link_with_id(&self, membrane: MembraneId) -> Vec<(LinkId, Link)> {
        self.links
            .iter()
            .filter(|(id, _)| id.parent() == membrane.id())
            .map(|(id, link)| (*id, link.clone()))
            .collect()
    }

    fn hyperlink_with_id(&self, membrane: MembraneId) -> Vec<(HyperLinkId, HyperLink)> {
        self.hyperlinks
            .iter()
            .filter(|(id, _)| id.parent() == membrane.id())
            .map(|(id, link)| (*id, link.clone()))
            .collect()
    }

    pub(crate) fn solve(&mut self) -> SolveResult {
        let root = self.root;
        let mut free = vec![];
        let mut res = self.solve_membrane(&root, &mut free);

        if !free.is_empty() {
            res.errors.push(TransformError::UnconstrainedLink);
        }

        res
    }

    fn solve_membrane(
        &mut self,
        membrane_id: &MembraneId,
        free_links: &mut Vec<Link>,
    ) -> SolveResult {
        let membrane = self.membranes.get_mut(membrane_id).unwrap().clone();
        let mut result = SolveResult::default();
        for mem in &membrane.membranes {
            result.combine(self.solve_membrane(mem, free_links));
        }

        for rule in membrane.rules.iter() {
            let rule = self.rules.get_mut(rule).unwrap();
            result.combine(rule.solve());
        }

        for atom_id in &membrane.atoms {
            let atom = self.atoms.get(atom_id).unwrap();
            for (idx, arg) in atom.args.iter().enumerate() {
                if let Process::Link(id) = arg {
                    let link = self.links.get_mut(id).unwrap();
                    if link.arg1.is_none() {
                        link.arg1 = Some((Process::Atom(*atom_id), idx));
                    } else if link.arg2.is_none() {
                        link.arg2 = Some((Process::Atom(*atom_id), idx));
                    } else {
                        result.errors.push(TransformError::LinkTooManyOccurrence);
                    }
                }
            }
        }

        struct Update {
            atom: AtomId,
            pos: usize,
            process: Process,
        }

        let mut updates = vec![];

        for atom_id in &membrane.atoms {
            let atom = self.atoms.get_mut(atom_id).unwrap();
            for arg in atom.args.iter_mut() {
                if let Process::Link(id) = arg {
                    let link = self.links.get(id).unwrap();
                    if let Some((Process::Atom(id), _)) = &link.arg1 {
                        if id == atom_id {
                            if let Some((p, _)) = &link.arg2 {
                                *arg = *p;
                            } else {
                                // the other side of the link is not connected
                                // so we need to connect it to a free link from inner membranes if possible
                                for link_ in free_links.iter_mut() {
                                    // same name
                                    if link_.name == link.name {
                                        if let Some((Process::Atom(other_id), pos)) = link_.arg1 {
                                            *arg = Process::Atom(other_id);
                                            updates.push(Update {
                                                atom: other_id,
                                                pos,
                                                process: Process::Atom(*atom_id),
                                            });
                                            link_.arg1 = None;
                                        }
                                    }
                                }
                            }
                        } else {
                            *arg = Process::Atom(*id);
                        }
                    }
                }
            }
        }

        for update in updates {
            let atom = self.atoms.get_mut(&update.atom).unwrap();
            atom.args[update.pos] = update.process;
        }

        let mut remove = vec![];

        for link_id in membrane.links.iter() {
            let link = self.links.get(link_id).unwrap();
            match (link.arg1, link.arg2) {
                (None, None) | (None, Some(_)) => unreachable!(),
                (Some(_), None) => {
                    free_links.push(link.clone());
                }
                (Some(_), Some(_)) => {
                    // both sides are connected
                    // so we don't need this link anymore
                    remove.push(*link_id);
                }
            }
        }

        let membrane = self.membranes.get_mut(membrane_id).unwrap();
        for link_id in remove {
            self.links.remove(&link_id);
            membrane.links.remove(&link_id);
        }

        free_links.retain(|link| link.arg1.is_some());

        result
    }
}

impl Membrane {
    pub(crate) fn add_processes(&mut self, processes: Vec<Process>, storage: &impl Storage) {
        for process in processes {
            self.add_process(&process, storage);
        }
    }

    fn add_process(&mut self, process: &Process, storage: &impl Storage) {
        match process {
            Process::Atom(id) => {
                if !self.atoms.insert(*id) {
                    return;
                }
                let atom = storage.get_atom(*id).unwrap();
                for arg in atom.args.iter() {
                    self.add_process(arg, storage);
                }
            }
            Process::Membrane(id) => {
                self.membranes.insert(*id);
            }
            Process::Link(id) => {
                self.links.insert(*id);
            }
            Process::Hyperlink(id) => {
                self.hyperlinks.insert(*id);
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
            Self::Membrane(id) => id.id().into(),
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
