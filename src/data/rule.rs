use crate::transform::ProcessHolder;

use super::{
    guard::{Guard, ProcessConstraint},
    Atom, Link, Membrane, Process,
};

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

#[derive(Debug, Default)]
pub struct Rule {
    pub name: String,
    /// Raw head, use `pattern` to get the solved head
    pub head: Vec<Process>,
    /// Processes to retain after the application of the rule
    pub propagation: Vec<Process>,
    /// Guards, with order preserved
    pub guard: Guard,
    /// Raw body, use `pattern` to get the solved body
    pub body: Vec<Process>,

    pattern_parsed: bool,

    pattern_atom_store: Vec<Atom>,
    pattern_membrane_store: Vec<Membrane>,

    link_store: Vec<RuleLink>,

    body_atom_store: Vec<Atom>,
    body_membrane_store: Vec<Membrane>,
}

impl Rule {
    pub fn set_pattern_parsed(&mut self) {
        self.pattern_parsed = true;
    }

    pub(crate) fn get_link(&self, as_str: &str) -> Process {
        for (i, link) in self.link_store.iter().enumerate() {
            if link.name == as_str {
                return Process::Link(i);
            }
        }
        panic!("link {} not defined", as_str);
    }

    pub(crate) fn get_link_by_name_mut(&mut self, as_str: &str) -> &mut RuleLink {
        for link in &mut self.link_store {
            if link.name == as_str {
                return link;
            }
        }
        panic!("link {} has not been defined", as_str);
    }

    pub(crate) fn get_link_by_id_mut(&mut self, id: usize) -> &mut RuleLink {
        &mut self.link_store[id]
    }

    /// Register a definition and return its id
    ///
    /// Return `None` if the definition is already registered or the name is invalid
    pub(crate) fn register_def(&mut self, as_str: &str, ty: ProcessConstraint) -> usize {
        for link in &self.link_store {
            if link.name == as_str {
                panic!("link {} already defined", as_str);
            }
        }
        let id = self.link_store.len();
        let mut link = RuleLink::def(as_str.to_string());
        link.type_ = Some(ty);
        self.link_store.push(link);
        id
    }

    /// Compare the head of two rules
    ///
    /// Rules are considered equal if their names and heads are 'equal'
    pub fn compare_head(&self, other: &Rule) -> bool {
        self.name == other.name && self.head == other.head
    }
}

impl ProcessHolder for Rule {
    fn add_atom(&mut self, atom: Atom) -> usize {
        if self.pattern_parsed {
            self.body_atom_store.push(atom);
            self.body_atom_store.len() - 1
        } else {
            self.pattern_atom_store.push(atom);
            self.pattern_atom_store.len() - 1
        }
    }

    fn add_membrane(&mut self, membrane: Membrane) -> usize {
        if self.pattern_parsed {
            self.body_membrane_store.push(membrane);
            self.body_membrane_store.len() - 1
        } else {
            self.pattern_membrane_store.push(membrane);
            self.pattern_membrane_store.len() - 1
        }
    }

    fn add_link(&mut self, link: Link) -> usize {
        if self.pattern_parsed {
            for (i, l) in self.link_store.iter_mut().enumerate() {
                if l.name == link.name {
                    if !l.def {
                        l.linked_in_body = true;
                    }
                    return i;
                }
            }
            panic!("link {} not found", link.name)
        } else {
            for (i, l) in self.link_store.iter().enumerate() {
                if l.name == link.name {
                    return i;
                }
            }

            self.link_store.push(link.into());
            self.link_store.len() - 1
        }
    }

    fn alpha_connect(&mut self, process: Process, other: Process) {
        match process {
            Process::Atom(id1) => match other {
                Process::Atom(id2) => {
                    if self.pattern_parsed {
                        self.body_atom_store[id1].args.push(other);
                        self.body_atom_store[id2].args.push(process);
                    } else {
                        self.pattern_atom_store[id1].args.push(other);
                        self.pattern_atom_store[id2].args.push(process);
                    }
                }
                Process::Membrane(_) => unimplemented!(),
                Process::Link(_) => {
                    if self.pattern_parsed {
                        self.body_atom_store[id1].args.push(other);
                    } else {
                        self.pattern_atom_store[id1].args.push(other);
                    }
                }
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Membrane(_) => unimplemented!(),
            Process::Link(_) => match other {
                Process::Atom(id2) => {
                    if self.pattern_parsed {
                        self.body_atom_store[id2].args.push(process);
                    } else {
                        self.pattern_atom_store[id2].args.push(process);
                    }
                }
                Process::Membrane(_) => unimplemented!(),
                Process::Link(_) => {
                    unimplemented!("Alpha conversion between links is not supported yet")
                }
                Process::Hyperlink(_) => unimplemented!(),
            },
            Process::Hyperlink(_) => unimplemented!(),
        }
    }

    fn get_atom_mut(&mut self, id: usize) -> &mut Atom {
        if self.pattern_parsed {
            &mut self.body_atom_store[id]
        } else {
            &mut self.pattern_atom_store[id]
        }
    }

    fn get_membrane_mut(&mut self, id: usize) -> &mut Membrane {
        if self.pattern_parsed {
            &mut self.body_membrane_store[id]
        } else {
            &mut self.pattern_membrane_store[id]
        }
    }

    fn get_atom(&self, id: usize) -> &Atom {
        if self.pattern_parsed {
            &self.body_atom_store[id]
        } else {
            &self.pattern_atom_store[id]
        }
    }

    fn get_membrane(&self, id: usize) -> &Membrane {
        if self.pattern_parsed {
            &self.body_membrane_store[id]
        } else {
            &self.pattern_membrane_store[id]
        }
    }
}

/// Implementation about pattern resolution
impl Rule {
    pub fn solve(&mut self) {
        // link resolution
        self.solve_links();
    }

    fn solve_links(&mut self) {
        for (atom_idx, atom) in self.pattern_atom_store.iter().enumerate() {
            for (arg_idx, arg) in atom.args.iter().enumerate() {
                if let Process::Link(id) = arg {
                    let link = &mut self.link_store[*id];
                    if link.arg1 == RuleLinkArg::None {
                        link.arg1 = RuleLinkArg::Head(Process::Atom(atom_idx), arg_idx);
                    } else if link.arg2 == RuleLinkArg::None {
                        link.arg2 = RuleLinkArg::Head(Process::Atom(atom_idx), arg_idx);
                    } else {
                        panic!("link {} has too many arguments", link.name);
                    }
                }
            }
        }

        for (atom_idx, atom) in self.body_atom_store.iter().enumerate() {
            for (arg_idx, arg) in atom.args.iter().enumerate() {
                if let Process::Link(id) = arg {
                    let link = &mut self.link_store[*id];
                    if link.def {
                        link.arg1 = RuleLinkArg::Temp(*id);
                    }
                    if link.arg1 == RuleLinkArg::None {
                        link.arg1 = RuleLinkArg::Body(Process::Atom(atom_idx), arg_idx);
                    } else if link.arg2 == RuleLinkArg::None {
                        link.arg2 = RuleLinkArg::Body(Process::Atom(atom_idx), arg_idx);
                    } else {
                        panic!("link {} has too many arguments", link.name);
                    }
                }
            }
        }

        for link in &self.link_store {
            use RuleLinkStatus::*;
            if let Plain = link.status() {
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
                if head {
                    {
                        let arg1 = &mut self.pattern_atom_store[arg1.get_id()];
                        arg1.args[idx1] = Process::Atom(arg2.get_id());
                    }
                    let arg2 = &mut self.pattern_atom_store[arg2.get_id()];
                    arg2.args[idx2] = Process::Atom(arg1.get_id());
                } else {
                    {
                        let arg1 = &mut self.body_atom_store[arg1.get_id()];
                        arg1.args[idx1] = Process::Atom(arg2.get_id());
                    }
                    let arg2 = &mut self.body_atom_store[arg2.get_id()];
                    arg2.args[idx2] = Process::Atom(arg1.get_id());
                }
            }
        }
    }
}
