use core::panic;
use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use crate::transform::{SolveResult, TransformError};

use super::guard::{GuardNode, GuardSource, ProcessConstraint, VariableId};
use super::{guard::Guard, Atom, Data, Link, Membrane};

use super::id::*;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RuleAtom {
    parent: MembraneId,
    name: String,
    data: Data,
    type_: Option<ProcessConstraint>,
    args: Vec<RuleLink>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RuleHyperlink {
    name: String,
    first_in_head: bool,
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
    hyperlinks: HashMap<HyperlinkId, RuleHyperlink>,
    var_atoms: HashMap<VariableId, Vec<AtomId>>,
    vars: HashMap<AtomId, RuleAtom>,

    id_generator: IdGenerator,
}

/// Argument of a link in a rule
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuleLinkArg {
    /// Not linked to any atom yet
    None,
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
    pub opposite: RuleLinkArg,
}

impl RuleHyperlink {
    pub(crate) fn first_in_head(&self) -> bool {
        self.first_in_head
    }
}

impl RuleAtom {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> impl Iterator<Item = &RuleLink> {
        self.args.iter()
    }

    pub fn data(&self) -> &Data {
        &self.data
    }

    pub fn type_(&self) -> Option<ProcessConstraint> {
        self.type_
    }
}

impl RuleHyperlink {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> impl Iterator<Item = &RuleLink> {
        self.args.iter()
    }
}

impl RuleLink {
    fn from(link: &Link, this_in_head: bool, op_in_head: bool) -> Self {
        Self {
            name: link.name.clone(),
            this: if this_in_head {
                RuleLinkArg::Head(link.this.0, link.this.1)
            } else {
                RuleLinkArg::Body(link.this.0, link.this.1)
            },
            opposite_type: None,
            opposite: match &link.opposite {
                Some(opposite) => {
                    if op_in_head {
                        RuleLinkArg::Head(opposite.0, opposite.1)
                    } else {
                        RuleLinkArg::Body(opposite.0, opposite.1)
                    }
                }
                None => RuleLinkArg::None,
            },
        }
    }
}

#[allow(dead_code)]
impl RuleLinkArg {
    pub(crate) fn id_index(&self) -> Option<(AtomId, usize)> {
        match self {
            Self::Head(id, index) | Self::Temp(id, index) | Self::Body(id, index) => {
                Some((*id, *index))
            }
            RuleLinkArg::None => None,
        }
    }

    pub(crate) fn atom_id(&self) -> Option<AtomId> {
        match self {
            Self::Head(id, _) | Self::Temp(id, _) | Self::Body(id, _) => Some(*id),
            RuleLinkArg::None => None,
        }
    }

    pub(crate) fn index(&self) -> Option<usize> {
        match self {
            Self::Head(_, index) | Self::Temp(_, index) | Self::Body(_, index) => Some(*index),
            RuleLinkArg::None => None,
        }
    }

    pub(crate) fn is_none(&self) -> bool {
        matches!(self, RuleLinkArg::None)
    }

    pub(crate) fn is_temp(&self) -> bool {
        matches!(self, RuleLinkArg::Temp(_, _))
    }

    pub(crate) fn is_head(&self) -> bool {
        matches!(self, RuleLinkArg::Head(_, _))
    }

    pub(crate) fn is_body(&self) -> bool {
        matches!(self, RuleLinkArg::Body(_, _))
    }

    pub(crate) fn is_linked(&self) -> bool {
        !matches!(self, RuleLinkArg::None)
    }
}

impl Display for RuleLinkArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Head(id, index) => write!(f, "Head({}, {})", id, index),
            Self::Temp(id, index) => write!(f, "Temp({}, {})", id, index),
            Self::Body(id, index) => write!(f, "Body({}, {})", id, index),
            RuleLinkArg::None => write!(f, "None"),
        }
    }
}

impl Rule {
    pub(crate) fn next_membrane_id(&mut self) -> MembraneId {
        self.id_generator.next_membrane_id()
    }

    pub(crate) fn next_atom_id(&mut self, parent: MembraneId) -> AtomId {
        self.id_generator.next_atom_id(parent)
    }

    pub(crate) fn temp_link_name(&mut self) -> String {
        let id = self.id_generator.next_link_id();
        format!("~temp{}", id)
    }

    pub(crate) fn add_membrane(&mut self, id: MembraneId, membrane: Membrane) {
        if self.head_membranes.contains(&membrane.parent) {
            self.head_membranes.insert(id);
        } else if self.body_membranes.contains(&membrane.parent) {
            self.body_membranes.insert(id);
        }
        self.membranes.insert(id, membrane);
    }

    pub(crate) fn add_atom(&mut self, id: AtomId, atom: Atom) {
        let atom = RuleAtom {
            parent: atom.parent,
            name: atom.name,
            type_: match &atom.data {
                Data::Int(_) => Some(ProcessConstraint::Int),
                Data::Float(_) => Some(ProcessConstraint::Float),
                _ => None,
            },
            data: atom.data,
            args: atom
                .args
                .iter()
                .map(|link| {
                    let head = {
                        if let Some((id, _)) = link.opposite {
                            if let Some(hl) = self.hyperlinks.get(&id) {
                                hl.first_in_head
                            } else {
                                !self.head_parsed
                            }
                        } else {
                            !self.head_parsed
                        }
                    };
                    RuleLink::from(link, !self.head_parsed, head)
                })
                .collect(),
        };
        self.atoms.insert(id, atom);
    }

    #[allow(unused_variables)]
    pub(crate) fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> super::RuleId {
        unimplemented!("Generating rules in rules is not supported")
    }

    pub(crate) fn add_hyperlink(&mut self, name: &str) -> super::HyperlinkId {
        for (id, link_) in &mut self.hyperlinks {
            if link_.name == name {
                return *id;
            }
        }

        let id = self.id_generator.next_hyperlink_id();
        let hyperlink = RuleHyperlink {
            name: name.to_owned(),
            first_in_head: !self.head_parsed,
            args: vec![],
        };
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    pub(crate) fn get_atom_arg_len(&self, id: AtomId) -> usize {
        self.atoms.get(&id).map_or(0, |atom| atom.args.len())
    }

    pub(crate) fn get_hyperlink_arg_len(&self, id: HyperlinkId) -> usize {
        self.hyperlinks.get(&id).map_or(0, |link| link.args.len())
    }

    pub(crate) fn append_atom_arg(&mut self, id: AtomId, link: Link) {
        self.atoms.get_mut(&id).unwrap().args.push(RuleLink::from(
            &link,
            !self.head_parsed,
            !self.head_parsed,
        ));
    }

    pub(crate) fn append_hyperlink_arg(&mut self, id: HyperlinkId, link: Link) {
        self.hyperlinks
            .get_mut(&id)
            .unwrap()
            .args
            .push(RuleLink::from(&link, !self.head_parsed, !self.head_parsed));
    }
}

/// Helper struct for solving rules
#[derive(Debug, Clone, Copy)]
struct LinkInfo {
    this: RuleLinkArg,
    this_type: Option<ProcessConstraint>,
    opposite: RuleLinkArg,
}

impl Rule {
    pub(crate) fn new(name: String, mem_id: MembraneId) -> Self {
        Self {
            name,
            parent: mem_id,
            ..Default::default()
        }
    }

    pub(crate) fn all_atoms(&self) -> &HashMap<AtomId, RuleAtom> {
        &self.atoms
    }

    pub(crate) fn head_atoms(&self) -> HashMap<AtomId, &RuleAtom> {
        self.membranes[&self.head]
            .atoms
            .iter()
            .map(move |id| (*id, &self.atoms[id]))
            .collect()
    }

    pub(crate) fn var_atoms(&self) -> HashMap<AtomId, &RuleAtom> {
        self.vars.iter().map(|(id, atom)| (*id, atom)).collect()
    }

    pub(crate) fn body_atoms(&self) -> HashMap<AtomId, &RuleAtom> {
        self.membranes[&self.body]
            .atoms
            .iter()
            .map(move |id| (*id, &self.atoms[id]))
            .collect()
    }

    pub(crate) fn hyperlinks(&self) -> HashMap<HyperlinkId, &RuleHyperlink> {
        self.hyperlinks
            .iter()
            .map(|(id, link)| (*id, link))
            .collect()
    }

    pub(crate) fn is_in_head(&self, id: MembraneId) -> bool {
        self.head_membranes.contains(&id)
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

    pub(super) fn solve(&mut self) -> SolveResult {
        let mut free = HashMap::new();

        self.unification(self.head);

        let head = self.head;
        let mut res = self.solve_membrane(&head, &mut free, false);

        if let Some(propagation) = self.propagation {
            res.combine(self.solve_membrane(&propagation, &mut free, false));
        }

        self.solve_guard(&mut free);

        let body = self.body;
        res.combine(self.solve_membrane(&body, &mut free, true));

        self.unification(self.body);

        if !free.is_empty() {
            res.errors.push(TransformError::UnconstrainedLink);
        }

        res
    }

    fn solve_membrane(
        &mut self,
        membrane_id: &MembraneId,
        free_links: &mut HashMap<String, LinkInfo>,
        body: bool,
    ) -> SolveResult {
        let membrane = self.membranes.get_mut(membrane_id).unwrap().clone();
        let mut result = SolveResult::default();
        for mem in &membrane.membranes {
            result.combine(self.solve_membrane(mem, free_links, body));
        }

        // fix links

        let mut connected = HashSet::new();

        // contains update info for links after solving
        let mut updates = vec![];

        for atom_id in &membrane.atoms {
            let atom = self.atoms.get_mut(atom_id).unwrap();
            for link in atom.args.iter_mut() {
                if let Some(id) = link.opposite.atom_id() {
                    if self.hyperlinks.contains_key(&id) {
                        link.opposite_type = Some(ProcessConstraint::Hyperlink);
                    }
                } else {
                    if let Some(var_id) = self.guard.defined(&link.name) {
                        // create new atom for every variable
                        let id = self.id_generator.next_atom_id(self.parent);
                        let var = self.guard.get(var_id).unwrap();
                        let name = if let Some(ty) = var.ty {
                            match ty {
                                ProcessConstraint::Int => "_int",
                                ProcessConstraint::Float => "_float",
                                ProcessConstraint::String => todo!(),
                                _ => unreachable!(),
                            }
                        } else {
                            &link.name
                        };
                        let atom = RuleAtom {
                            parent: self.parent,
                            name: name.to_string(),
                            data: Data::Variable(var_id),
                            type_: None,
                            args: vec![],
                        };
                        self.var_atoms.entry(var_id).or_default().push(id);
                        self.vars.insert(id, atom);
                        link.opposite = RuleLinkArg::Temp(id, 0);
                    }
                    if connected.contains(&link.name) {
                        // link with the same name already connected
                        result.errors.push(TransformError::LinkTooManyOccurrence);
                    } else if body {
                        // handle constrainted links in body
                        if let Some(cor_link) = free_links.get_mut(&link.name) {
                            if cor_link.this_type.is_some() {
                                link.opposite = cor_link.opposite;
                            } else if let Some(other) = free_links.insert(
                                link.name.clone(),
                                LinkInfo {
                                    this: link.opposite,
                                    this_type: link.opposite_type,
                                    opposite: link.this,
                                },
                            ) {
                                updates.push((other.opposite, link.this));
                                connected.insert(link.name.clone());
                            }
                        } else if let Some(other) = free_links.insert(
                            link.name.clone(),
                            LinkInfo {
                                this: link.opposite,
                                this_type: link.opposite_type,
                                opposite: link.this,
                            },
                        ) {
                            updates.push((other.opposite, link.this));
                            connected.insert(link.name.clone());
                        }
                    } else {
                        // link with the same name not connected
                        if let Some(other) = free_links.insert(
                            link.name.clone(),
                            LinkInfo {
                                this: link.opposite,
                                this_type: link.opposite_type,
                                opposite: link.this,
                            },
                        ) {
                            updates.push((other.opposite, link.this));
                            connected.insert(link.name.clone());
                        }
                    }
                }
            }
        }

        // remove free links from connected

        for name in connected {
            free_links.remove(&name);
        }

        for (from, to) in updates {
            if let Some((id, idx)) = from.id_index() {
                let atom = self.atoms.get_mut(&id).unwrap();
                atom.args[idx].opposite = to;
            }
            if let Some((id, idx)) = to.id_index() {
                let atom = self.atoms.get_mut(&id).unwrap();
                atom.args[idx].opposite = from;
            }
        }

        result
    }

    fn solve_guard(&mut self, free_links: &mut HashMap<String, LinkInfo>) {
        let mut guard = self.guard.clone();

        for constraint in guard.constraints.iter_mut() {
            match constraint {
                GuardNode::Constraint(func, vars) => {
                    'var: for var in vars.iter_mut() {
                        if let GuardSource::Placeholder(name) = var {
                            if let Some(link) = free_links.get_mut(name) {
                                if let Some((id, idx)) = link.opposite.id_index() {
                                    let atom = self.atoms.get_mut(&id).unwrap();
                                    atom.args[idx].opposite_type = Some(*func);
                                    link.this_type = Some(*func);
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

        for def in guard.definitions.iter_mut() {
            // do special handling for definitions with only one item
            let var = def.1;
            match &mut var.node {
                GuardNode::Var(var) => {
                    if let GuardSource::Placeholder(name) = var {
                        if let Some(link) = free_links.get_mut(name) {
                            if let Some((id, idx)) = link.opposite.id_index() {
                                *var = GuardSource::AtPortOfAtom(id, idx);
                            }
                            continue; // continue to next definition, otherwise it is unconstrained
                        }
                        panic!("Unconstrained link in guard");
                    }
                }
                GuardNode::Binary(op, lhs, rhs) => {
                    let op = *op;
                    let _left =
                        self.substitute_link_in_guard_with_type(lhs, &op.into(), free_links);
                    let _right =
                        self.substitute_link_in_guard_with_type(rhs, &op.into(), free_links);
                    var.ty = Some(op.into());
                }
                GuardNode::Function(sig, args) => {
                    var.ty = Some(sig.ret);
                    for arg in args {
                        let _ =
                            self.substitute_link_in_guard_with_type(arg, &sig.args[0], free_links);
                    }
                }
                GuardNode::Constraint(..) => unreachable!(),
                GuardNode::Int(_) => {
                    var.ty = Some(ProcessConstraint::Int);
                }
                GuardNode::Float(_) => {
                    var.ty = Some(ProcessConstraint::Float);
                }
            }
        }

        self.guard = guard;
    }

    fn substitute_link_in_guard_with_type(
        &mut self,
        node: &mut GuardNode,
        type_: &ProcessConstraint,
        free_links: &mut HashMap<String, LinkInfo>,
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
                            for link in free_links.values_mut() {
                                if link.opposite == atom.args[*idx].this {
                                    link.this_type = Some(*type_);
                                }
                            }
                        }
                    }
                    GuardSource::Variable(def) => {
                        if let Some(atoms) = self.var_atoms.get(def) {
                            for atom in atoms {
                                let atom = self.atoms.get_mut(atom).unwrap();
                                if let Some(opposite_type) = atom.type_ {
                                    if opposite_type != *type_ {
                                        panic!("Type mismatch in guard");
                                    }
                                } else {
                                    atom.type_ = Some(*type_);
                                }
                            }
                        }
                    }
                    GuardSource::Placeholder(name) => {
                        if let Some(link) = free_links.get_mut(name) {
                            if let Some((id, idx)) = link.opposite.id_index() {
                                let atom = self.atoms.get_mut(&id).unwrap();
                                atom.args[idx].opposite_type = Some(*type_);
                                link.this_type = Some(*type_);
                                substitute = Some(GuardSource::AtPortOfAtom(id, idx));
                            }
                        }
                        for hl in self.hyperlinks.values() {
                            if hl.name == *name {
                                let op = hl.args[0].opposite;
                                let (id, idx) = op.id_index().unwrap();
                                substitute = Some(GuardSource::AtPortOfAtom(id, idx));
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
            GuardNode::Constraint(_, _) => {
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
            GuardNode::Function(sig, args) => {
                for arg in args {
                    let _ = self.substitute_link_in_guard_with_type(arg, &sig.args[0], free_links);
                }
                sig.ret
            }
        }
    }

    fn unification(&mut self, mem_id: MembraneId) {
        let membrane = self.membranes.get(&mem_id).unwrap().clone();

        for mem in &membrane.membranes {
            self.unification(*mem);
        }

        let mut connectors = vec![];
        let mut remove = vec![];

        for atom_id in &membrane.atoms {
            let atom = self.atoms.get(atom_id).unwrap();
            if atom.name == "=" && atom.args.len() == 2 {
                connectors.push(atom_id);
            }
        }

        for connector in &connectors {
            let atom = self.atoms.get_mut(connector).unwrap();
            let left = atom.args[0].opposite;
            let left_type = atom.args[0].opposite_type;
            let right = atom.args[1].opposite;
            let right_type = atom.args[1].opposite_type;
            let left_name = atom.args[0].name.clone();
            let right_name = atom.args[1].name.clone();

            if atom.parent == self.body {
                // in body but both sides are in head
                if left.is_head() && right.is_head() {
                    continue;
                }
            }

            match (left.id_index(), right.id_index()) {
                (Some((id, idx)), None) => {
                    if !self.vars.contains_key(&id) {
                        let atom = self.atoms.get_mut(&id).unwrap();
                        atom.args[idx].opposite = right;
                        atom.args[idx].opposite_type = right_type;
                        atom.args[idx].name = right_name;
                    }
                }
                (None, Some((id, idx))) => {
                    if !self.vars.contains_key(&id) {
                        let atom = self.atoms.get_mut(&id).unwrap();
                        atom.args[idx].opposite = left;
                        atom.args[idx].opposite_type = left_type;
                        atom.args[idx].name = left_name;
                    }
                }
                (Some((left_id, left_idx)), Some((right_id, right_idx))) => {
                    if !self.vars.contains_key(&left_id) {
                        let left_atom = self.atoms.get_mut(&left_id).unwrap();
                        left_atom.args[left_idx].opposite = right;
                        left_atom.args[left_idx].opposite_type = right_type;
                        left_atom.args[left_idx].name = right_name;
                    }
                    if !self.vars.contains_key(&right_id) {
                        let right_atom = self.atoms.get_mut(&right_id).unwrap();
                        right_atom.args[right_idx].opposite = left;
                        right_atom.args[right_idx].opposite_type = left_type;
                    }
                }
                (None, None) => {}
            }

            remove.push(*connector);
        }

        let membrane = self.membranes.get_mut(&mem_id).unwrap();

        for connector in remove {
            self.atoms.remove(connector);
            membrane.atoms.remove(connector);
        }
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for id in &self.membranes[&self.head].atoms {
            let atom = &self.atoms[id];
            write!(f, "[{}]{}(", id, atom.name)?;
            let args = atom
                .args
                .iter()
                .map(|link| link.name.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{}),", args)?;
        }

        write!(f, " :- ")?;

        for id in &self.membranes[&self.body].atoms {
            let atom = &self.atoms[id];
            write!(f, "[{}]{}(", id, atom.name)?;
            let args = atom
                .args
                .iter()
                .map(|link| link.name.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{}),", args)?;
        }

        write!(f, ".")
    }
}
