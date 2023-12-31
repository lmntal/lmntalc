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
    hyperlinks: HashMap<HyperlinkId, Hyperlink>,

    id_generator: id::IdGenerator,
}

impl Program {
    pub(crate) fn set_root(&mut self, root: MembraneId) {
        self.root = root;
    }

    pub fn root(&self) -> MembraneId {
        self.root
    }

    pub fn atoms(&self, membrane: MembraneId) -> HashMap<AtomId, &Atom> {
        self.membranes.get(&membrane).map_or(HashMap::new(), |mem| {
            mem.atoms.iter().map(|id| (*id, &self.atoms[id])).collect()
        })
    }

    pub fn membranes(&self, membrane: MembraneId) -> Vec<&Membrane> {
        self.membranes.get(&membrane).map_or(Vec::new(), |mem| {
            mem.membranes.iter().map(|id| &self.membranes[id]).collect()
        })
    }

    pub fn hyperlinks(&self) -> &HashMap<HyperlinkId, Hyperlink> {
        &self.hyperlinks
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
}

impl Program {
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

        let mut connectors = vec![];

        // fix links

        let mut links = free_links
            .iter()
            .map(|link| (link.name.clone(), link.this))
            .collect::<HashMap<_, _>>();
        let mut connected = HashSet::new();
        let mut updates = vec![];

        for atom_id in &membrane.atoms {
            let atom = self.atoms.get_mut(atom_id).unwrap();
            if atom.name == "=" && atom.args.len() == 2 {
                connectors.push(*atom_id);
            }
            for link in &atom.args {
                if link.opposite.is_none() {
                    if connected.contains(&link.name) {
                        // link with the same name already connected
                        result.errors.push(TransformError::LinkTooManyOccurrence);
                    } else {
                        // link with the same name not connected
                        if let Some(other) = links.insert(link.name.clone(), link.this) {
                            updates.push((other, link.this));
                            connected.insert(link.name.clone());
                        }
                    }
                }
            }
        }

        for (from, to) in updates {
            let atom = self.atoms.get_mut(&from.0).unwrap();
            atom.args[from.1].opposite = Some(to);
            let atom = self.atoms.get_mut(&to.0).unwrap();
            atom.args[to.1].opposite = Some(from);
        }

        free_links.extend(links.into_iter().map(|(name, this)| Link {
            name,
            this,
            opposite: None,
        }));

        // unification

        let mem = self.membranes.get_mut(membrane_id).unwrap();

        for connector in connectors {
            let atom = self.atoms.get_mut(&connector).unwrap();
            let left = atom.args[0].opposite.unwrap();
            let right = atom.args[1].opposite.unwrap();
            let name = atom.args[1].name.clone();

            {
                let atom = self.atoms.get_mut(&left.0).unwrap();
                atom.args[left.1].opposite = Some(right);
                atom.args[left.1].name = name.clone();
            }
            {
                let atom = self.atoms.get_mut(&right.0).unwrap();
                atom.args[right.1].opposite = Some(left);
            }

            mem.atoms.remove(&connector);
            self.atoms.remove(&connector);
        }

        for rule in membrane.rules.iter() {
            let rule = self.rules.get_mut(rule).unwrap();
            result.combine(rule.solve());
        }

        result
    }
}

impl Storage for Program {
    fn next_membrane_id(&mut self) -> MembraneId {
        self.id_generator.next_membrane_id()
    }

    fn next_atom_id(&mut self, parent: MembraneId) -> AtomId {
        self.id_generator.next_atom_id(parent)
    }

    fn temp_link_name(&mut self) -> String {
        let id = self.id_generator.next_link_id();
        format!("~{}", id.id())
    }

    fn add_atom(&mut self, id: AtomId, atom: Atom) {
        self.atoms.insert(id, atom);
    }

    fn add_membrane(&mut self, id: MembraneId, membrane: Membrane) {
        self.membranes.insert(id, membrane);
    }

    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> RuleId {
        let id = self.id_generator.next_rule_id(parent);
        self.rules.insert(id, rule);
        id
    }

    fn add_hyperlink(&mut self, name: &str) -> HyperlinkId {
        for (id, hyperlink_) in &self.hyperlinks {
            if name == hyperlink_.name {
                return *id;
            }
        }
        let id = self.id_generator.next_hyperlink_id();
        let hyperlink = Hyperlink {
            name: name.to_string(),
            ..Default::default()
        };
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    fn append_atom_arg(&mut self, id: AtomId, link: Link) {
        if let Some(atom) = self.atoms.get_mut(&id) {
            atom.args.push(link);
        }
    }

    fn append_hyperlink_arg(&mut self, id: HyperlinkId, link: Link) {
        if let Some(hyperlink) = self.hyperlinks.get_mut(&id) {
            hyperlink.args.push(link);
        }
    }

    fn get_atom_arg_len(&self, id: AtomId) -> usize {
        self.atoms.get(&id).map_or(0, |atom| atom.args.len())
    }

    fn get_hyperlink_arg_len(&self, id: HyperlinkId) -> usize {
        self.hyperlinks
            .get(&id)
            .map_or(0, |hyperlink| hyperlink.args.len())
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
