use std::collections::HashMap;

use crate::transform::{SolveResult, Storage, TransformError, TransformWarning};

use super::{
    guard::{Guard, ProcessConstraint},
    Atom, HyperLink, Link, Membrane, Process,
};

use super::id::*;

#[derive(Debug, Default, Clone)]
pub struct Rule {
    /// Parent membrane
    pub parent: MembraneId,
    /// Name of the rule
    pub name: String,
    /// Head
    pub head: Membrane,
    /// Propagation, with order preserved
    pub propagation: Membrane,
    /// Guards, with order preserved
    pub guard: Guard,
    /// Body
    pub body: Membrane,

    head_parsed: bool,
    mem_counter: u32,

    head_membranes: HashMap<MembraneId, Membrane>,
    head_atoms: HashMap<AtomId, Atom>,

    links: HashMap<LinkId, RuleLink>,
    hyperlinks: HashMap<HyperLinkId, HyperLink>,

    body_membranes: HashMap<MembraneId, Membrane>,
    body_atoms: HashMap<AtomId, Atom>,

    id_generator: IdGenerator,
}

/// Argument of a link in a rule
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuleLinkArg {
    /// Not linked
    None,
    /// Appears in the head of the rule,
    /// with the position of the link in the `Process`
    Head(Process, usize),
    /// Temporary variable, like `X` in `X <- Y`
    Temp(LinkId),
    /// Appears in the body of the rule,
    /// with the position of the link in the `Process`
    Body(Process, usize),
}

/// Link used in a rule
///
/// The difference between `Link` and `RuleLink` is that
/// `RuleLink` contains information about the link's position
/// in the rule, and the type of the link (the process at the
/// other end of the link).
#[derive(Debug, Clone)]
pub struct RuleLink {
    pub(crate) name: String,
    pub(crate) arg1: RuleLinkArg,
    pub(crate) arg2: RuleLinkArg,
    pub(crate) type_: Option<ProcessConstraint>,
    pub(crate) linked_in_body: bool,
    pub(crate) def: bool,
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

impl Storage for Rule {
    fn add_atom(&mut self, atom: Atom, parent: MembraneId) -> AtomId {
        let id = self.id_generator.next_atom_id(parent);
        if self.head_parsed {
            self.body_atoms.insert(id, atom);
        } else {
            self.head_atoms.insert(id, atom);
        }
        id
    }

    fn next_membrane_id(&mut self) -> MembraneId {
        self.mem_counter += 1;
        MembraneId::new(self.mem_counter - 1)
    }

    fn add_membrane(&mut self, id: MembraneId, membrane: Membrane) {
        if self.head_parsed {
            self.body_membranes.insert(id, membrane);
        } else {
            self.head_membranes.insert(id, membrane);
        }
        self.mem_counter += 1;
    }

    #[allow(unused_variables)]
    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> super::RuleId {
        unimplemented!("Generating rules in rules is not supported")
    }

    fn add_link(&mut self, link: Link, parent: MembraneId) -> LinkId {
        if self.head_parsed {
            for (id, link_) in &mut self.links {
                if link_.name == link.name {
                    if !link_.def {
                        link_.linked_in_body = true;
                    }
                    return *id;
                }
            }

            let id = self.id_generator.next_link_id(parent);
            let link = RuleLink::from(link);
            self.links.insert(id, link);
            id
        } else {
            for (id, link_) in &mut self.links {
                if link_.name == link.name {
                    return *id;
                }
            }

            let id = self.id_generator.next_link_id(parent);
            let link = RuleLink::from(link);
            self.links.insert(id, link);
            id
        }
    }

    fn add_hyperlink(&mut self, hyperlink: HyperLink, parent: MembraneId) -> super::HyperLinkId {
        for (id, link_) in &mut self.hyperlinks {
            if link_.name == hyperlink.name {
                return *id;
            }
        }

        let id = self.id_generator.next_hyperlink_id(parent);
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    fn get_atom(&self, id: AtomId) -> Option<&Atom> {
        if self.head_parsed {
            self.body_atoms.get(&id)
        } else {
            self.head_atoms.get(&id)
        }
    }

    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom> {
        if self.head_parsed {
            self.body_atoms.get_mut(&id)
        } else {
            self.head_atoms.get_mut(&id)
        }
    }

    fn alpha_connect(&mut self, left: Process, right: Process) {
        match (left, right) {
            (Process::Atom(atom1), Process::Atom(atom2)) => {
                if self.head_parsed {
                    self.body_atoms.get_mut(&atom1).unwrap().args.push(right);
                    self.body_atoms.get_mut(&atom2).unwrap().args.push(left);
                } else {
                    self.head_atoms.get_mut(&atom1).unwrap().args.push(right);
                    self.head_atoms.get_mut(&atom2).unwrap().args.push(left);
                }
            }
            (Process::Atom(atom), Process::Link(_)) => {
                if self.head_parsed {
                    self.body_atoms.get_mut(&atom).unwrap().args.push(right);
                } else {
                    self.head_atoms.get_mut(&atom).unwrap().args.push(right);
                }
            }
            (Process::Link(_), Process::Atom(atom)) => {
                if self.head_parsed {
                    self.body_atoms.get_mut(&atom).unwrap().args.push(left);
                } else {
                    self.head_atoms.get_mut(&atom).unwrap().args.push(left);
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl RuleLink {
    pub fn new(name: String) -> Self {
        RuleLink {
            name,
            arg1: RuleLinkArg::None,
            arg2: RuleLinkArg::None,
            type_: Option::None,
            linked_in_body: false,
            def: false,
        }
    }

    pub fn def(name: String) -> Self {
        RuleLink {
            name,
            arg1: RuleLinkArg::None,
            arg2: RuleLinkArg::None,
            type_: Option::None,
            linked_in_body: false,
            def: true,
        }
    }

    pub fn status(&self) -> RuleLinkStatus {
        match (self.arg1, self.arg2) {
            (RuleLinkArg::None, RuleLinkArg::None)
            | (RuleLinkArg::None, _)
            | (RuleLinkArg::Body(_, _), RuleLinkArg::Head(_, _))
            | (RuleLinkArg::Head(_, _), RuleLinkArg::Temp(_))
            | (RuleLinkArg::Temp(_), RuleLinkArg::Head(_, _))
            | (RuleLinkArg::Temp(_), RuleLinkArg::Temp(_))
            | (RuleLinkArg::Body(_, _), RuleLinkArg::Temp(_)) => unreachable!(),

            (RuleLinkArg::Head(_, _), RuleLinkArg::None) => {
                if self.type_.is_some() {
                    RuleLinkStatus::Guarded
                } else {
                    RuleLinkStatus::Unconstrained
                }
            }

            (RuleLinkArg::Head(_, _), RuleLinkArg::Head(_, _))
            | (RuleLinkArg::Body(_, _), RuleLinkArg::Body(_, _)) => RuleLinkStatus::Plain,

            (RuleLinkArg::Head(_, _), RuleLinkArg::Body(_, _)) => {
                RuleLinkStatus::LinkedInHeadAndBody
            }
            (RuleLinkArg::Body(_, _), RuleLinkArg::None) => {
                if self.def {
                    RuleLinkStatus::Guarded
                } else {
                    RuleLinkStatus::Unconstrained
                }
            }
            (RuleLinkArg::Temp(_), RuleLinkArg::None) => RuleLinkStatus::Unused,
            (RuleLinkArg::Temp(_), RuleLinkArg::Body(_, _)) => RuleLinkStatus::Guarded, // Asserted to be guarded
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arg1(&self) -> RuleLinkArg {
        self.arg1
    }

    pub fn arg2(&self) -> RuleLinkArg {
        self.arg2
    }

    pub fn is_linked_in_body(&self) -> bool {
        self.linked_in_body
    }

    pub fn get_type(&self) -> Option<ProcessConstraint> {
        self.type_
    }
}

impl From<Link> for RuleLink {
    fn from(link: Link) -> Self {
        RuleLink {
            name: link.name,
            arg1: RuleLinkArg::None,
            arg2: RuleLinkArg::None,
            type_: Option::None,
            linked_in_body: false,
            def: false,
        }
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

    pub(crate) fn get_link(&self, as_str: &str) -> Process {
        for (id, link) in &self.links {
            if link.name == as_str {
                return Process::Link(*id);
            }
        }
        panic!("link {} has not been defined", as_str);
    }

    pub(crate) fn get_link_by_name_mut(&mut self, as_str: &str) -> &mut RuleLink {
        for link in &mut self.links.values_mut() {
            if link.name == as_str {
                return link;
            }
        }
        panic!("link {} has not been defined", as_str);
    }

    pub(crate) fn get_link_by_id_mut(&mut self, id: LinkId) -> &mut RuleLink {
        self.links.get_mut(&id).unwrap()
    }

    /// Register a definition and return its id
    ///
    /// Return `None` if the definition is already registered or the name is invalid
    pub(crate) fn register_def(&mut self, as_str: &str, ty: ProcessConstraint) -> LinkId {
        for link in self.links.values() {
            if link.name == as_str {
                panic!("link {} already defined", as_str);
            }
        }
        let id = self.id_generator.next_link_id(self.parent);
        let mut link = RuleLink::def(as_str.to_string());
        link.type_ = Some(ty);
        self.links.insert(id, link);
        id
    }

    pub fn head_atoms(&self) -> &HashMap<AtomId, Atom> {
        &self.head_atoms
    }

    pub fn head_atoms_mut(&mut self) -> &mut HashMap<AtomId, Atom> {
        &mut self.head_atoms
    }

    pub fn head_membranes(&self) -> &HashMap<MembraneId, Membrane> {
        &self.head_membranes
    }

    pub fn head_membranes_mut(&mut self) -> &mut HashMap<MembraneId, Membrane> {
        &mut self.head_membranes
    }

    pub fn body_atoms(&self) -> &HashMap<AtomId, Atom> {
        &self.body_atoms
    }

    pub fn body_atoms_mut(&mut self) -> &mut HashMap<AtomId, Atom> {
        &mut self.body_atoms
    }

    pub fn body_membranes(&self) -> &HashMap<MembraneId, Membrane> {
        &self.body_membranes
    }

    pub fn body_membranes_mut(&mut self) -> &mut HashMap<MembraneId, Membrane> {
        &mut self.body_membranes
    }

    pub fn links(&self) -> &HashMap<LinkId, RuleLink> {
        &self.links
    }

    pub fn links_mut(&mut self) -> &mut HashMap<LinkId, RuleLink> {
        &mut self.links
    }

    pub fn hyper_links(&self) -> &HashMap<HyperLinkId, HyperLink> {
        &self.hyperlinks
    }

    pub fn hyper_links_mut(&mut self) -> &mut HashMap<HyperLinkId, HyperLink> {
        &mut self.hyperlinks
    }

    pub(super) fn solve(&mut self) -> SolveResult {
        let mut free_links = vec![];
        let mut result = SolveResult::default();

        let head = self.head.clone();
        for mem in &head.membranes {
            result.combine(self.solve_head_membrane(mem, &mut free_links));
        }
        result.combine(self.solve_head_link(&head, &mut free_links));

        let body = self.body.clone();
        for mem in &body.membranes {
            result.combine(self.solve_body_membrane(mem, &mut free_links));
        }
        result.combine(self.solve_body_link(&body, &mut free_links));

        for link in self.links.values() {
            use RuleLinkStatus::*;
            match link.status() {
                Unused => {
                    result.warnings.push(TransformWarning::UnusedVariable);
                }
                Unconstrained => {
                    result.errors.push(TransformError::UnconstrainedLink);
                }
                Plain => {
                    let head = match link.arg1 {
                        RuleLinkArg::Head(_, _) => true,
                        RuleLinkArg::Body(_, _) => false,
                        RuleLinkArg::None => unreachable!(),
                        RuleLinkArg::Temp(_) => todo!(),
                    };
                    let (arg1, idx1) = match link.arg1 {
                        RuleLinkArg::Head(proc, idx) => (proc, idx),
                        RuleLinkArg::Body(proc, idx) => (proc, idx),
                        RuleLinkArg::None => unreachable!(),
                        RuleLinkArg::Temp(_) => todo!(),
                    };
                    let (arg2, idx2) = match link.arg2 {
                        RuleLinkArg::Head(proc, idx) => (proc, idx),
                        RuleLinkArg::Body(proc, idx) => (proc, idx),
                        RuleLinkArg::None => unreachable!(),
                        RuleLinkArg::Temp(_) => todo!(),
                    };
                    let arg1_id: AtomId = arg1.get_id().into();
                    let arg2_id: AtomId = arg2.get_id().into();
                    if head {
                        {
                            let arg1 = &mut self.head_atoms.get_mut(&arg1_id).unwrap();
                            arg1.args[idx1] = Process::Atom(arg2_id);
                        }
                        let arg2 = &mut self.head_atoms.get_mut(&arg2_id).unwrap();
                        arg2.args[idx2] = Process::Atom(arg1_id);
                    } else {
                        {
                            let arg1 = &mut self.body_atoms.get_mut(&arg1_id).unwrap();
                            arg1.args[idx1] = Process::Atom(arg2_id);
                        }
                        let arg2 = &mut self.body_atoms.get_mut(&arg2_id).unwrap();
                        arg2.args[idx2] = Process::Atom(arg1_id);
                    }
                }
                _ => {}
            }
        }

        result
    }

    fn solve_head_membrane(
        &mut self,
        membrane_id: &MembraneId,
        free_links: &mut Vec<RuleLink>,
    ) -> SolveResult {
        let membrane = self.head_membranes.get_mut(membrane_id).unwrap().clone();
        let mut result = SolveResult::default();
        for mem in &membrane.membranes {
            result.combine(self.solve_head_membrane(mem, free_links));
        }
        result.combine(self.solve_head_link(&membrane, free_links));

        result
    }

    fn solve_head_link(
        &mut self,
        membrane: &Membrane,
        free_links: &mut Vec<RuleLink>,
    ) -> SolveResult {
        let mut result = SolveResult::default();
        for atom_id in &membrane.atoms {
            let atom = self.head_atoms.get(atom_id).unwrap();
            for (idx, arg) in atom.args.iter().enumerate() {
                if let Process::Link(id) = arg {
                    let link = self.links.get_mut(id).unwrap();
                    if link.arg1 == RuleLinkArg::None {
                        link.arg1 = RuleLinkArg::Head(Process::Atom(*atom_id), idx);
                    } else if link.arg2 == RuleLinkArg::None {
                        link.arg2 = RuleLinkArg::Head(Process::Atom(*atom_id), idx);
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
            let atom = self.head_atoms.get_mut(atom_id).unwrap();
            for arg in atom.args.iter_mut() {
                if let Process::Link(id) = arg {
                    let link = self.links.get(id).unwrap();
                    if let RuleLinkArg::Head(Process::Atom(id), _) = &link.arg1 {
                        if id == atom_id {
                            if let RuleLinkArg::Head(p, _) = &link.arg2 {
                                *arg = *p;
                            } else {
                                // the other side of the link is not connected
                                // so we need to connect it to a free link from inner membranes if possible
                                for link_ in free_links.iter_mut() {
                                    // same name
                                    if link_.name == link.name {
                                        if let RuleLinkArg::Head(Process::Atom(other_id), pos) =
                                            link_.arg1
                                        {
                                            *arg = Process::Atom(other_id);
                                            updates.push(Update {
                                                atom: other_id,
                                                pos,
                                                process: Process::Atom(*atom_id),
                                            });
                                            link_.arg1 = RuleLinkArg::None;
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
            let atom = self.head_atoms.get_mut(&update.atom).unwrap();
            atom.args[update.pos] = update.process;
        }

        let mut remove = vec![];

        for link_id in membrane.links.iter() {
            let link = self.links.get(link_id).unwrap();
            if let (RuleLinkArg::Head(_, _), RuleLinkArg::Head(_, _)) = (link.arg1, link.arg2) {
                remove.push(*link_id);
            }
        }

        for link_id in remove {
            self.links.remove(&link_id);
        }

        free_links.retain(|link| link.arg1 != RuleLinkArg::None);

        result
    }

    fn solve_body_membrane(
        &mut self,
        membrane_id: &MembraneId,
        free_links: &mut Vec<RuleLink>,
    ) -> SolveResult {
        let membrane = self.body_membranes.get_mut(membrane_id).unwrap().clone();
        let mut result = SolveResult::default();

        for mem in &membrane.membranes {
            result.combine(self.solve_body_membrane(mem, free_links));
        }
        result.combine(self.solve_body_link(&membrane, free_links));

        result
    }

    fn solve_body_link(
        &mut self,
        membrane: &Membrane,
        free_links: &mut Vec<RuleLink>,
    ) -> SolveResult {
        let mut result = SolveResult::default();
        for atom_id in &membrane.atoms {
            let atom = self.body_atoms.get(atom_id).unwrap();
            for (idx, arg) in atom.args.iter().enumerate() {
                if let Process::Link(id) = arg {
                    let link = self.links.get_mut(id).unwrap();
                    if link.def {
                        link.arg1 = RuleLinkArg::Temp(*id);
                    }
                    if link.arg1 == RuleLinkArg::None {
                        link.arg1 = RuleLinkArg::Body(Process::Atom(*atom_id), idx);
                    } else if link.arg2 == RuleLinkArg::None {
                        link.arg2 = RuleLinkArg::Body(Process::Atom(*atom_id), idx);
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
            let atom = self.body_atoms.get_mut(atom_id).unwrap();
            for arg in atom.args.iter_mut() {
                if let Process::Link(id) = arg {
                    let link = self.links.get(id).unwrap();
                    if let RuleLinkArg::Body(Process::Atom(id), _) = &link.arg1 {
                        if id == atom_id {
                            if let RuleLinkArg::Body(p, _) = &link.arg2 {
                                *arg = *p;
                            } else {
                                // the other side of the link is not connected
                                // so we need to connect it to a free link from inner membranes if possible
                                for link_ in free_links.iter_mut() {
                                    // same name
                                    if link_.name == link.name {
                                        if let RuleLinkArg::Body(Process::Atom(other_id), pos) =
                                            link_.arg1
                                        {
                                            *arg = Process::Atom(other_id);
                                            updates.push(Update {
                                                atom: other_id,
                                                pos,
                                                process: Process::Atom(*atom_id),
                                            });
                                            link_.arg1 = RuleLinkArg::None;
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
            let atom = self.body_atoms.get_mut(&update.atom).unwrap();
            atom.args[update.pos] = update.process;
        }

        let mut remove = vec![];

        for link_id in membrane.links.iter() {
            let link = self.links.get(link_id).unwrap();
            if let (RuleLinkArg::Body(_, _), RuleLinkArg::Body(_, _)) = (link.arg1, link.arg2) {
                remove.push(*link_id);
            }
        }

        for link_id in remove {
            self.links.remove(&link_id);
        }

        free_links.retain(|link| link.arg1 != RuleLinkArg::None);

        result
    }
}
