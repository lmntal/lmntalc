use std::collections::HashMap;

use crate::transform::Storage;

use super::{
    guard::{Guard, ProcessConstraint},
    Atom, AtomId, HyperLinkId, Link, LinkId, Membrane, MembraneId, Process,
};

#[derive(Debug, Default)]
pub struct Rule {
    /// Parent membrane
    pub parent: MembraneId,
    /// Name of the rule
    pub name: String,
    /// Head, use `pattern` to get the solved head
    pub head: Vec<Process>,
    /// Propagation, with order preserved
    pub propagation: Vec<Process>,
    /// Guards, with order preserved
    pub guard: Guard,
    /// Raw body, use `pattern` to get the solved body
    pub body: Vec<Process>,

    pattern_parsed: bool,

    pattern_atom_store: HashMap<LinkId, Atom>,
    pattern_membrane_store: HashMap<LinkId, Membrane>,

    link_store: HashMap<LinkId, RuleLink>,
    hyper_link_store: HashMap<HyperLinkId, super::HyperLink>,

    body_atom_store: HashMap<LinkId, Atom>,
    body_membrane_store: HashMap<LinkId, Membrane>,
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
    Temp(usize),
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
    fn add_atom(&mut self, atom: Atom) -> AtomId {
        if self.pattern_parsed {
            let id = uuid::Uuid::new_v4();
            self.body_atom_store.insert(LinkId(id), atom);
            AtomId(id)
        } else {
            let id = uuid::Uuid::new_v4();
            self.pattern_atom_store.insert(LinkId(id), atom);
            AtomId(id)
        }
    }

    fn add_membrane(&mut self, membrane: Membrane) -> MembraneId {
        if self.pattern_parsed {
            let id = uuid::Uuid::new_v4();
            self.body_membrane_store.insert(LinkId(id), membrane);
            MembraneId(id)
        } else {
            let id = uuid::Uuid::new_v4();
            self.pattern_membrane_store.insert(LinkId(id), membrane);
            MembraneId(id)
        }
    }

    fn add_rule(&mut self, rule: Rule) -> super::RuleId {
        unimplemented!("Generating rules in rules is not supported")
    }

    fn add_link(&mut self, link: Link) -> LinkId {
        if self.pattern_parsed {
            for (id, link_) in &mut self.link_store {
                if link_.name == link.name {
                    if !link_.def {
                        link_.linked_in_body = true;
                    }
                    return *id;
                }
            }

            let id = LinkId(uuid::Uuid::new_v4());
            let link = RuleLink::from(link);
            self.link_store.insert(id, link);
            id
        } else {
            for (id, link_) in &mut self.link_store {
                if link_.name == link.name {
                    return *id;
                }
            }

            let id = LinkId(uuid::Uuid::new_v4());
            let link = RuleLink::from(link);
            self.link_store.insert(id, link);
            id
        }
    }

    fn add_hyperlink(&mut self, hyperlink: super::HyperLink) -> super::HyperLinkId {
        for (id, link_) in &mut self.hyper_link_store {
            if link_.name == hyperlink.name {
                return *id;
            }
        }

        let id = super::HyperLinkId(uuid::Uuid::new_v4());
        self.hyper_link_store.insert(id, hyperlink);
        id
    }

    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom> {
        if self.pattern_parsed {
            self.body_atom_store.get_mut(&LinkId(id.0))
        } else {
            self.pattern_atom_store.get_mut(&LinkId(id.0))
        }
    }

    fn get_membrane_mut(&mut self, id: MembraneId) -> Option<&mut Membrane> {
        if self.pattern_parsed {
            self.body_membrane_store.get_mut(&LinkId(id.0))
        } else {
            self.pattern_membrane_store.get_mut(&LinkId(id.0))
        }
    }

    fn alpha_connect(&mut self, left: Process, right: Process) {
        match (left, right) {
            (Process::Atom(atom1), Process::Atom(atom2)) => {
                if self.pattern_parsed {
                    self.body_atom_store
                        .get_mut(&LinkId(atom1.0))
                        .unwrap()
                        .args
                        .push(right);
                    self.body_atom_store
                        .get_mut(&LinkId(atom2.0))
                        .unwrap()
                        .args
                        .push(left);
                } else {
                    self.pattern_atom_store
                        .get_mut(&LinkId(atom1.0))
                        .unwrap()
                        .args
                        .push(right);
                    self.pattern_atom_store
                        .get_mut(&LinkId(atom2.0))
                        .unwrap()
                        .args
                        .push(left);
                }
            }
            (Process::Atom(atom), Process::Link(link)) => {
                if self.pattern_parsed {
                    self.body_atom_store
                        .get_mut(&LinkId(atom.0))
                        .unwrap()
                        .args
                        .push(right);
                } else {
                    self.pattern_atom_store
                        .get_mut(&LinkId(atom.0))
                        .unwrap()
                        .args
                        .push(right);
                }
            }
            (Process::Link(link), Process::Atom(atom)) => {
                if self.pattern_parsed {
                    self.body_atom_store
                        .get_mut(&LinkId(atom.0))
                        .unwrap()
                        .args
                        .push(left);
                } else {
                    self.pattern_atom_store
                        .get_mut(&LinkId(atom.0))
                        .unwrap()
                        .args
                        .push(left);
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
    pub fn set_pattern_parsed(&mut self) {
        self.pattern_parsed = true;
    }

    pub(crate) fn get_link(&self, as_str: &str) -> Process {
        for (id, link) in &self.link_store {
            if link.name == as_str {
                return Process::Link(*id);
            }
        }
        panic!("link {} has not been defined", as_str);
    }

    pub(crate) fn get_link_by_name_mut(&mut self, as_str: &str) -> &mut RuleLink {
        for (id, link) in &mut self.link_store {
            if link.name == as_str {
                return link;
            }
        }
        panic!("link {} has not been defined", as_str);
    }

    pub(crate) fn get_link_by_id_mut(&mut self, id: LinkId) -> &mut RuleLink {
        self.link_store.get_mut(&id).unwrap()
    }

    /// Register a definition and return its id
    ///
    /// Return `None` if the definition is already registered or the name is invalid
    pub(crate) fn register_def(&mut self, as_str: &str, ty: ProcessConstraint) -> LinkId {
        for (id, link) in &self.link_store {
            if link.name == as_str {
                panic!("link {} already defined", as_str);
            }
        }
        let id = LinkId(uuid::Uuid::new_v4());
        let mut link = RuleLink::def(as_str.to_string());
        link.type_ = Some(ty);
        self.link_store.insert(id, link);
        id
    }
}
