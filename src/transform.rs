mod guard;

use crate::{
    ast::{ASTNode, AtomName},
    data::{rule::Rule, *},
    token::Operator,
};

use self::guard::visit_guard;
use super::data::id::*;

pub trait Storage {
    fn add_atom(&mut self, atom: Atom, parent: MembraneId) -> AtomId;
    fn add_membrane(&mut self, membrane: Membrane, parent: MembraneId) -> MembraneId;
    fn add_rule(&mut self, rule: Rule, parent: MembraneId) -> RuleId;
    fn add_link(&mut self, link: Link, parent: MembraneId) -> LinkId;
    fn add_hyperlink(&mut self, hyperlink: HyperLink, parent: MembraneId) -> HyperLinkId;

    fn get_atom_mut(&mut self, id: AtomId) -> Option<&mut Atom>;
    fn get_membrane_mut(&mut self, id: MembraneId) -> Option<&mut Membrane>;

    fn alpha_connect(&mut self, left: Process, right: Process);
}

/// Transform LMNtal source code into a membrane (initial membrane)
pub fn transform_lmntal(ast: &ASTNode) -> Program {
    let mut program = Program::default();
    if let ASTNode::Membrane {
        name,
        process_lists,
        rules,
        ..
    } = ast
    {
        // root means void, since the "root membrane" has no parent
        let void = MembraneId::root();
        let mem = Membrane::new(name.clone(), void);
        let root = program.add_membrane(mem, void);
        program.set_root(root);
        for process_list in process_lists {
            let process =
                visit_process_list(process_list, &mut program, Process::Membrane(root), root);
            program
                .get_membrane_mut(root)
                .unwrap()
                .add_processes(process);
        }
        for rule in rules {
            let rule = visit_rule(rule, Process::Membrane(root), root);
            let rule = program.add_rule(rule, root);
            program.get_membrane_mut(root).unwrap().add_rule(rule);
        }
    } else {
        unreachable!("visit_membrane called with non-membrane node")
    }
    program
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
        let mut rule = Rule::default();

        let head = visit_process_list(head, &mut rule, from, mem_id);
        for process in head {
            if let Process::Link(_) = process {
                continue;
            }
            rule.head.push(process);
        }

        if let Some(propagation) = propagation {
            let propagation = visit_process_list(propagation, &mut rule, from, mem_id);
            for process in propagation {
                if let Process::Link(_) = process {
                    continue;
                }
                rule.propagation.push(process);
            }
        }

        rule.set_pattern_parsed();

        if let Some(guard) = guard {
            rule.guard = visit_guard(&mut rule, guard);
        }

        if let Some(body) = body {
            let body = visit_process_list(body, &mut rule, from, mem_id);
            rule.body = body;
        }
        // keep only letters in the name
        let mut name = name.clone();
        name.retain(|c| c.is_alphabetic());
        rule.name = name;
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
                let left = visit_process(left, store, from, mem_id);
                let right = &args[1];
                let right = visit_process(right, store, from, mem_id);
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

        if let Process::Atom(..) = from {
            processes.push(from);
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
        let mem = Membrane::new(name.clone(), mem_id);
        let proc = store.add_membrane(mem, mem_id);
        // TODO: proxy links
        for process_list in process_lists {
            let process = visit_process_list(process_list, store, Process::Membrane(proc), proc);
            store.get_membrane_mut(proc).unwrap().add_processes(process);
        }
        for rule in rules {
            let rule = visit_rule(rule, Process::Membrane(proc), proc);
            let rule = store.add_rule(rule, mem_id);
            store.get_membrane_mut(proc).unwrap().add_rule(rule);
        }
        proc
    } else {
        unreachable!("visit_membrane called with non-membrane node")
    }
}
