mod guard;

use crate::{
    ast::{ASTNode, AtomName},
    data::{rule::Rule, *},
    token::Operator,
};

use self::guard::visit_guard;
use super::data::id::*;

pub(crate) trait Storage {
    fn next_membrane_id(&mut self) -> MembraneId;

    fn add_atom(&mut self, atom: Atom, parent: MembraneId) -> AtomId;
    fn add_membrane(&mut self, id: MembraneId, membrane: Membrane);
    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> RuleId;
    fn add_link(&mut self, link: Link, parent: MembraneId) -> LinkId;
    fn add_hyperlink(&mut self, hyperlink: HyperLink, parent: MembraneId) -> HyperLinkId;

    fn get_atom(&self, id: AtomId) -> Option<&Atom>;
    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom>;

    fn alpha_connect(&mut self, left: Process, right: Process);
}

#[derive(Debug)]
pub enum TransformError {
    TopLevelLink,
    UnconstrainedLink,
    LinkTooManyOccurrence,
}

#[derive(Debug)]
pub enum TransformWarning {
    UnusedVariable,
}

#[derive(Debug)]
pub struct TransformResult {
    pub program: Program,
    pub warnings: Vec<TransformWarning>,
    pub errors: Vec<TransformError>,
}

#[derive(Debug, Default)]
pub(crate) struct SolveResult {
    pub warnings: Vec<TransformWarning>,
    pub errors: Vec<TransformError>,
}

impl SolveResult {
    pub(crate) fn combine(&mut self, other: SolveResult) {
        self.warnings.extend(other.warnings);
        self.errors.extend(other.errors);
    }
}

/// Transform LMNtal source code into a membrane (initial membrane)
pub fn transform_lmntal(ast: &ASTNode) -> TransformResult {
    let mut program = Program::default();
    if let ASTNode::Membrane {
        name,
        process_lists,
        rules,
        ..
    } = ast
    {
        // root means void, since the "root membrane" has no parent
        let void = MembraneId::void();
        let mut mem = Membrane::new(name.clone(), void);
        let root = program.next_membrane_id();
        program.set_root(root);
        for process_list in process_lists {
            let processes =
                visit_process_list(process_list, &mut program, Process::Membrane(root), root);
            mem.add_processes(processes, &program);
        }
        for rule in rules {
            let rule = visit_rule(rule, Process::Membrane(root), root);
            let rule = program.add_rule(rule, root);
            mem.add_rule(rule);
        }
        program.add_membrane(root, mem);
    } else {
        unreachable!("visit_membrane called with non-membrane node")
    }

    let err_warn = program.solve();

    TransformResult {
        program,
        warnings: err_warn.warnings,
        errors: err_warn.errors,
    }
}

/// Visit a rule node and transform it into a `Rule` struct
fn visit_rule(node: &ASTNode, from: Process, mem_id: MembraneId) -> Rule {
    if let ASTNode::Rule {
        name,
        head,
        propagation,
        body,
        guard,
        ..
    } = node
    {
        // keep only letters and underlscore in the name
        let mut name = name.clone();
        name.retain(|c| c.is_alphabetic());
        let mut rule = Rule::new(name, mem_id);
        let head_id = MembraneId::head();
        let head_procs = visit_process_list(head, &mut rule, from, head_id);
        let mut head = Membrane::new("_head".to_string(), mem_id);
        head.add_processes(head_procs, &rule);
        rule.head = head;

        if let Some(propagation) = propagation {
            let propagation_id = MembraneId::propagation();
            let propagation_procs =
                visit_process_list(propagation, &mut rule, from, propagation_id);
            let mut propagation = Membrane::new("_propagation".to_string(), mem_id);
            propagation.add_processes(propagation_procs, &rule);
            rule.propagation = propagation;
        }

        rule.set_head_parsed();

        if let Some(guard) = guard {
            rule.guard = visit_guard(&mut rule, guard);
        }

        if let Some(body) = body {
            let body_id = MembraneId::body();
            let body_procs = visit_process_list(body, &mut rule, from, body_id);
            let mut body = Membrane::new("_body".to_string(), mem_id);
            body.add_processes(body_procs, &rule);
            rule.body = body;
        }

        rule
    } else {
        unreachable!("visit_rule called with non-rule node")
    }
}

/// Visit a process list node and transform it into a vector of processes
fn visit_process_list(
    node: &ASTNode,
    store: &mut impl Storage,
    from: Process,
    mem_id: MembraneId,
) -> Vec<Process> {
    let mut proc_list = vec![];
    if let ASTNode::ProcessList { processes, .. } = node {
        for process in processes {
            if let ASTNode::Atom {
                name: AtomName::Operator(Operator::Equal),
                args,
                ..
            } = process
            {
                // do alpha conversion
                let left = &args[0];
                let left = visit_process(left, store, Process::Atom(u64::MAX.into()), mem_id);
                let right = &args[1];
                let right = visit_process(right, store, Process::Atom(u64::MAX.into()), mem_id);
                proc_list.push(left);
                proc_list.push(right);
                store.alpha_connect(left, right);
                continue;
            }
            proc_list.push(visit_process(process, store, from, mem_id));
        }
    } else {
        unreachable!("visit_membrane called with non-process node")
    }
    proc_list
}

fn visit_process(
    node: &ASTNode,
    store: &mut impl Storage,
    from: Process,
    mem_id: MembraneId,
) -> Process {
    match node {
        ASTNode::Atom { .. } => {
            let id = visit_atom(node, store, from, mem_id);
            Process::Atom(id)
        }
        ASTNode::Membrane { .. } => {
            let id = visit_membrane(node, store, from, mem_id);
            Process::Membrane(id)
        }
        ASTNode::Link {
            name, hyperlink, ..
        } => {
            match from {
                Process::Atom(_) => {}
                _ => {
                    panic!("top level link is not allowed");
                }
            }
            if *hyperlink {
                unimplemented!("hyperlink")
            } else {
                let link = Link::new(name);
                let id = store.add_link(link, mem_id);
                Process::Link(id)
            }
        }
        ASTNode::Context { .. } => {
            unimplemented!("context")
        }
        _ => unreachable!("visit_membrane called with non-process node"),
    }
}

/// Visit an atom node and add it to the holder
///
/// Returns the index of the atom in the holder
fn visit_atom(
    node: &ASTNode,
    store: &mut impl Storage,
    from: Process,
    mem_id: MembraneId,
) -> AtomId {
    if let ASTNode::Atom { name, args, .. } = node {
        let (name, data) = match name {
            AtomName::Plain(s) | AtomName::Keyword(s) => (s.to_string(), Data::Empty),
            AtomName::Operator(op) => (op.to_string(), Data::Empty),
            AtomName::Int(i) => (i.to_string(), Data::Int(*i)),
            AtomName::Float(f) => (f.to_string(), Data::Float(*f)),
            AtomName::Char(c) => (c.to_string(), Data::Char(*c)),
        };
        let atom = Atom {
            parent: mem_id,
            name: name.clone(),
            args: vec![],
            data,
        };
        let id = store.add_atom(atom, mem_id);

        let mut processes = vec![];

        if let Process::Atom(id) = from {
            if id != u64::MAX.into() {
                processes.push(from);
            }
        }

        for arg in args {
            processes.push(visit_process(arg, store, Process::Atom(id), mem_id));
        }
        let atom = store.get_atom_mut(id).unwrap();
        atom.args = processes;
        id
    } else {
        unreachable!("visit_atom called with non-atom node")
    }
}

/// Visit a membrane node and add it to the holder
///
/// Returns the index of the membrane in the holder
fn visit_membrane(
    node: &ASTNode,
    store: &mut impl Storage,
    _from: Process,
    mem_id: MembraneId,
) -> MembraneId {
    if let ASTNode::Membrane {
        name,
        process_lists,
        rules,
        ..
    } = node
    {
        let mut mem = Membrane::new(name.clone(), mem_id);
        let mem_id = store.next_membrane_id();

        for process_list in process_lists {
            let processes =
                visit_process_list(process_list, store, Process::Membrane(mem_id), mem_id);
            mem.add_processes(processes, store);
        }

        for rule in rules {
            let rule = visit_rule(rule, Process::Membrane(mem_id), mem_id);
            let rule = store.add_rule(rule, mem_id);
            mem.add_rule(rule);
        }

        store.add_membrane(mem_id, mem);

        mem_id
    } else {
        unreachable!("visit_membrane called with non-membrane node")
    }
}
