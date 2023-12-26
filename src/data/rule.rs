use core::panic;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use crate::transform::{SolveResult, Storage, TransformError};

use super::guard::{GuardNode, GuardSource, ProcessConstraint};
use super::{guard::Guard, Atom, Data, HyperLink, Link, Membrane};

use super::id::*;

#[derive(Debug, Clone)]
pub struct RuleAtom {
    parent: MembraneId,
    name: String,
    data: Data,
    variable: bool,
    type_: Option<ProcessConstraint>,
    hyperlink: bool,
    args: Vec<RuleLink>,
}

#[derive(Debug, Default, Clone)]
pub struct Rule {
    /// Parent membrane
    pub parent: MembraneId,
    /// Name of the rule
    pub name: String,
    /// Head
    pub head: MembraneId,
    /// Propagation, with order preserved
    pub propagation: Option<MembraneId>,
    /// Guards, with order preserved
    pub guard: Guard,
    /// Body
    pub body: MembraneId,

    head_parsed: bool,

    head_membranes: HashSet<MembraneId>,
    body_membranes: HashSet<MembraneId>,

    membranes: HashMap<MembraneId, Membrane>,
    atoms: HashMap<AtomId, RuleAtom>,
    hyperlinks: HashMap<HyperLinkId, HyperLink>,
    definitions: HashMap<AtomId, RuleAtom>,
    def_name: HashMap<String, AtomId>,

    id_generator: IdGenerator,
}

/// Argument of a link in a rule
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuleLinkArg {
    /// Appears in the head of the rule,
    /// with the position of the link in the `Process`
    Head(AtomId, usize),
    /// Temporary variable, like `X` in `X = Y + 1`
    Temp(AtomId, usize),
    /// Appears in the body of the rule,
    /// with the position of the link in the `Process`
    Body(AtomId, usize),
}

#[derive(Debug, Clone)]
pub struct RuleLink {
    pub name: String,
    pub this: RuleLinkArg,
    pub opposite_type: Option<ProcessConstraint>,
    pub opposite: Option<RuleLinkArg>,
}

/// Status of a link in a rule
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuleLinkStatus {
    /// Plain link
    Plain,
    /// Free link in head that will be linked to a free link in body
    LinkedInHeadAndBody,
    /// Type constrained link
    Guarded,
    /// Defined but not used, will raise warning
    Unused,
    /// Unconstrained free link, will raise error
    Unconstrained,
}

impl RuleLink {
    fn from(link: &Link, head: bool) -> Self {
        Self {
            name: link.name.clone(),
            this: if head {
                RuleLinkArg::Head(link.this.0, link.this.1)
            } else {
                RuleLinkArg::Body(link.this.0, link.this.1)
            },
            opposite_type: None,
            opposite: link.opposite.map(|opposite| {
                if head {
                    RuleLinkArg::Head(opposite.0, opposite.1)
                } else {
                    RuleLinkArg::Body(opposite.0, opposite.1)
                }
            }),
        }
    }
}

impl RuleLinkArg {
    fn atom_id(&self) -> AtomId {
        match self {
            Self::Head(id, _) | Self::Temp(id, _) | Self::Body(id, _) => *id,
        }
    }

    fn index(&self) -> usize {
        match self {
            Self::Head(_, index) | Self::Temp(_, index) | Self::Body(_, index) => *index,
        }
    }
}

impl Display for RuleLinkArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Head(id, index) => write!(f, "Head({}, {})", id, index),
            Self::Temp(id, index) => write!(f, "Temp({}, {})", id, index),
            Self::Body(id, index) => write!(f, "Body({}, {})", id, index),
        }
    }
}

impl Storage for Rule {
    fn next_membrane_id(&mut self) -> MembraneId {
        self.id_generator.next_membrane_id()
    }

    fn next_atom_id(&mut self, parent: MembraneId) -> AtomId {
        self.id_generator.next_atom_id(parent)
    }

    fn temp_link_name(&mut self) -> String {
        let id = self.id_generator.next_link_id();
        format!("~temp{}", id.id())
    }

    fn add_membrane(&mut self, id: MembraneId, membrane: Membrane) {
        if self.head_membranes.contains(&membrane.parent) {
            self.head_membranes.insert(id);
        } else if self.body_membranes.contains(&membrane.parent) {
            self.body_membranes.insert(id);
        }
        self.membranes.insert(id, membrane);
    }

    fn add_atom(&mut self, id: AtomId, atom: Atom) {
        let atom = RuleAtom {
            parent: atom.parent,
            name: atom.name,
            variable: false,
            type_: match &atom.data {
                Data::Int(_) => Some(ProcessConstraint::Int),
                Data::Float(_) => Some(ProcessConstraint::Float),
                _ => None,
            },
            data: atom.data,
            hyperlink: atom.hyperlink,
            args: atom
                .args
                .iter()
                .map(|link| RuleLink::from(link, !self.head_parsed))
                .collect(),
        };
        self.atoms.insert(id, atom);
    }

    #[allow(unused_variables)]
    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> super::RuleId {
        unimplemented!("Generating rules in rules is not supported")
    }

    fn add_hyperlink(&mut self, hyperlink: HyperLink) -> super::HyperLinkId {
        for (id, link_) in &mut self.hyperlinks {
            if link_.name == hyperlink.name {
                return *id;
            }
        }

        let id = self.id_generator.next_hyperlink_id();
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    fn get_atom_arg_len(&self, id: AtomId) -> usize {
        self.atoms.get(&id).map_or(0, |atom| atom.args.len())
    }

    fn append_atom_arg(&mut self, id: AtomId, link: Link) {
        self.atoms
            .get_mut(&id)
            .unwrap()
            .args
            .push(RuleLink::from(&link, !self.head_parsed));
    }
}

impl Rule {
    pub(crate) fn new(name: String, mem_id: MembraneId) -> Self {
        Self {
            name,
            parent: mem_id,
            ..Default::default()
        }
    }

    pub(crate) fn set_head_parsed(&mut self) {
        self.head_parsed = true;
    }

    pub(crate) fn set_head(&mut self, head: MembraneId) {
        self.head = head;
        self.head_membranes.insert(head);
    }

    pub(crate) fn set_propagation(&mut self, propagation: MembraneId) {
        self.propagation = Some(propagation);
        self.head_membranes.insert(propagation);
    }

    pub(crate) fn set_body(&mut self, body: MembraneId) {
        self.body = body;
        self.body_membranes.insert(body);
    }

    /// Register a definition and return its id
    ///
    /// Return `None` if the definition is already registered or the name is invalid
    pub(crate) fn register_def(&mut self, name: &str) -> AtomId {
        let id = self.id_generator.next_atom_id(self.parent);
        let atom = RuleAtom {
            parent: self.parent,
            name: name.to_owned(),
            data: Data::Empty,
            type_: None,
            variable: true,
            hyperlink: false,
            args: vec![],
        };
        self.definitions.insert(id, atom);
        self.def_name.insert(name.to_owned(), id);
        id
    }

    fn print(&self) {
        let mut str = String::new();
        for id in &self.membranes[&self.head].atoms {
            let atom = &self.atoms[id];
            str.push_str(&format!("[{}]{}(", id, atom.name));
            for link in &atom.args {
                str.push_str(&format!("{}<", link.name));
                str.push_str(&format!("{}", link.this));
                if let Some(opposite) = link.opposite {
                    str.push_str(&format!(", {}", opposite));
                }
                str.push_str(">, ");
            }
            str.push_str("), ");
        }

        str.push_str(" :- ");

        for id in &self.membranes[&self.body].atoms {
            let atom = &self.atoms[id];
            str.push_str(&format!("[{}]{}(", id, atom.name));
            for link in &atom.args {
                str.push_str(&format!("{}<", link.name));
                str.push_str(&format!("{}", link.this));
                if let Some(opposite) = link.opposite {
                    str.push_str(&format!(", {}", opposite));
                }
                str.push_str(">, ");
            }
            str.push_str("), ");
        }

        println!("{}", str);
    }

    pub(super) fn solve(&mut self) -> SolveResult {
        let mut free = vec![];

        let head = self.head;
        let mut res = self.solve_membrane(&head, &mut free);

        if let Some(propagation) = self.propagation {
            res.combine(self.solve_membrane(&propagation, &mut free));
        }

        self.solve_guard(&free);

        let body = self.body;
        res.combine(self.solve_membrane(&body, &mut free));

        self.unification();

        if !free.is_empty() {
            res.errors.push(TransformError::UnconstrainedLink);
        }

        res
    }

    fn solve_membrane(
        &mut self,
        membrane_id: &MembraneId,
        free_links: &mut Vec<RuleLink>,
    ) -> SolveResult {
        let membrane = self.membranes.get_mut(membrane_id).unwrap().clone();
        let mut result = SolveResult::default();
        for mem in &membrane.membranes {
            result.combine(self.solve_membrane(mem, free_links));
        }

        // fix links

        let mut links = free_links
            .iter()
            .map(|link| (link.name.clone(), link.this))
            .collect::<HashMap<_, _>>();
        let mut connected = HashSet::new();
        let mut updates = vec![];

        for atom_id in &membrane.atoms {
            let atom = self.atoms.get_mut(atom_id).unwrap();
            for link in atom.args.iter_mut() {
                if link.opposite.is_none() {
                    if let Some(def_id) = self.def_name.get(&link.name) {
                        link.opposite = Some(RuleLinkArg::Temp(*def_id, 0));
                    }
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
            let atom = self.atoms.get_mut(&from.atom_id()).unwrap();
            atom.args[from.index()].opposite = Some(to);
            let atom = self.atoms.get_mut(&to.atom_id()).unwrap();
            atom.args[to.index()].opposite = Some(from);
        }

        free_links.extend(links.into_iter().map(|(name, this)| RuleLink {
            name,
            this,
            opposite: None,
            opposite_type: None,
        }));

        result
    }

    fn solve_guard(&mut self, free_links: &[RuleLink]) {
        let mut guard = self.guard.clone();

        for constraint in guard.constraints.iter_mut() {
            match constraint {
                GuardNode::Func(func, vars) => {
                    'var: for var in vars.iter_mut() {
                        if let GuardSource::Placeholder(name) = var {
                            for link in free_links {
                                if link.name == *name {
                                    let atom = self.atoms.get_mut(&link.this.atom_id()).unwrap();
                                    atom.args[link.this.index()].opposite_type = Some(*func);
                                    continue 'var;
                                }
                            }
                        }
                    }
                }
                GuardNode::Binary(op, lhs, rhs) => {
                    let op = *op;
                    _ = self.substitute_link_in_guard_with_type(lhs, &op.into(), free_links);
                    _ = self.substitute_link_in_guard_with_type(rhs, &op.into(), free_links);
                }
                _ => {}
            }
        }

        'def: for def in guard.definitions.iter_mut() {
            // do special handling for definitions with only one item
            match def.1 {
                GuardNode::Var(var) => {
                    if let GuardSource::Placeholder(name) = var {
                        for link in free_links {
                            if link.name == *name {
                                *var = GuardSource::AtPortOfAtom(
                                    link.this.atom_id(),
                                    link.this.index(),
                                );
                                continue 'def; // continue to next definition, otherwise it is unconstrained
                            }
                        }
                        panic!("Unconstrained link in guard");
                    }
                }
                GuardNode::Int(i) => {
                    let atom = self.definitions.get_mut(def.0).unwrap();
                    atom.data = Data::Int(*i);
                }
                GuardNode::Float(f) => {
                    let atom = self.definitions.get_mut(def.0).unwrap();
                    atom.data = Data::Float(*f);
                }
                GuardNode::Binary(op, lhs, rhs) => {
                    let op = *op;
                    _ = self.substitute_link_in_guard_with_type(lhs, &op.into(), free_links);
                    _ = self.substitute_link_in_guard_with_type(rhs, &op.into(), free_links);
                }
                GuardNode::Func(..) => unreachable!(),
            }
        }

        self.guard = guard;
    }

    fn substitute_link_in_guard_with_type(
        &mut self,
        node: &mut GuardNode,
        type_: &ProcessConstraint,
        free_links: &[RuleLink],
    ) -> ProcessConstraint {
        match node {
            GuardNode::Var(var) => {
                let mut substitute = None;
                match var {
                    GuardSource::AtPortOfAtom(atom_id, idx) => {
                        let atom = self.atoms.get(atom_id).unwrap();
                        let link = &atom.args[*idx];
                        if let Some(opposite_type) = link.opposite_type {
                            if opposite_type != *type_ {
                                panic!("Type mismatch in guard");
                            }
                        } else {
                            let atom = self.atoms.get_mut(atom_id).unwrap();
                            atom.args[*idx].opposite_type = Some(*type_);
                        }
                    }
                    GuardSource::Definition(def) => {
                        let atom = self.definitions.get(def).unwrap();
                        if let Some(opposite_type) = atom.type_ {
                            if opposite_type != *type_ {
                                panic!("Type mismatch in guard");
                            }
                        } else {
                            let atom = self.definitions.get_mut(def).unwrap();
                            atom.type_ = Some(*type_);
                        }
                    }
                    GuardSource::Placeholder(name) => {
                        for link in free_links {
                            if link.name == *name {
                                let atom = self.atoms.get_mut(&link.this.atom_id()).unwrap();
                                atom.args[link.this.index()].opposite_type = Some(*type_);
                                substitute = Some(GuardSource::AtPortOfAtom(
                                    link.this.atom_id(),
                                    link.this.index(),
                                ));
                            }
                        }
                        if substitute.is_none() {
                            for (id, atom) in self.definitions.iter_mut() {
                                if atom.name == *name {
                                    atom.type_ = Some(*type_);
                                    substitute = Some(GuardSource::Definition(*id));
                                }
                            }
                        }
                    }
                }

                if let Some(substitute) = substitute {
                    *var = substitute;
                }

                *type_
            }
            GuardNode::Int(_) => ProcessConstraint::Int,
            GuardNode::Float(_) => ProcessConstraint::Float,
            GuardNode::Func(_, _) => {
                unreachable!("Function in expression is not supported yet")
            }
            GuardNode::Binary(op, lhs, rhs) => {
                let op = *op;
                let lhs_type = self.substitute_link_in_guard_with_type(lhs, &op.into(), free_links);
                let rhs_type = self.substitute_link_in_guard_with_type(rhs, &op.into(), free_links);
                if lhs_type != rhs_type {
                    panic!("Type mismatch in guard");
                }
                lhs_type
            }
        }
    }

    fn unification(&mut self) {
        let mut connectors = vec![];

        for (atom_id, atom) in &self.atoms {
            if atom.name == "=" && atom.args.len() == 2 {
                connectors.push(*atom_id);
            }
        }

        for connector in &connectors {
            let atom = self.atoms.get_mut(connector).unwrap();
            let left = atom.args[0].opposite.unwrap();
            let right = atom.args[1].opposite.unwrap();
            let name = atom.args[1].name.clone();

            if !self.definitions.contains_key(&left.atom_id()) {
                let atom = self.atoms.get_mut(&left.atom_id()).unwrap();
                atom.args[left.index()].opposite = Some(right);
                atom.args[left.index()].name = name.clone();
            }
            if !self.definitions.contains_key(&right.atom_id()) {
                let atom = self.atoms.get_mut(&right.atom_id()).unwrap();
                atom.args[right.index()].opposite = Some(left);
            }

            self.atoms.remove(connector);
            let head = self.membranes.get_mut(&self.head).unwrap();
            if !head.atoms.remove(connector) {
                let body = self.membranes.get_mut(&self.body).unwrap();
                body.atoms.remove(connector);
            }
        }
    }
}
