mod guard;
mod util;

use crate::{
    ast::ASTNode,
    data::{rule::Rule, Atom, Link, Membrane, Process},
};

use self::{guard::visit_guard, util::name_dispatch};

/// Holder means a data structure that can hold processes
///
/// Currently, it is implemented by Membrane and Rule
pub trait ProcessHolder {
    fn add_atom(&mut self, atom: Atom) -> usize;
    fn add_link(&mut self, link: Link) -> usize;
    fn add_membrane(&mut self, membrane: Membrane) -> usize;

    fn get_atom(&self, id: usize) -> &Atom;
    fn get_membrane(&self, id: usize) -> &Membrane;

    fn get_atom_mut(&mut self, id: usize) -> &mut Atom;
    fn get_membrane_mut(&mut self, id: usize) -> &mut Membrane;

    fn alpha_connect(&mut self, process: Process, other: Process);
}

/// Transform LMNtal source code into a membrane (initial membrane)
pub fn transform_lmntal(ast: &ASTNode) -> Membrane {
    if let ASTNode::Membrane {
        name,
        process_lists,
        rules,
        ..
    } = ast
    {
        let mut mem = Membrane::new(name.clone());
        for process_list in process_lists {
            let process = visit_process_list(process_list, &mut mem, Process::Membrane(0), 0);
            mem.add_processes(process);
        }
        for rule in rules {
            let rule = visit_rule(rule, Process::Membrane(0), 0);
            mem.add_rule(rule);
        }
        mem.solve();
        mem
    } else {
        panic!("visit_membrane called with non-membrane node")
    }
}

/// Visit a rule node and transform it into a `Rule` struct
fn visit_rule(node: &ASTNode, from: Process, mem_id: usize) -> Rule {
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
    holder: &mut impl ProcessHolder,
    from: Process,
    mem_id: usize,
) -> Vec<Process> {
    let mut proc_list = vec![];
    if let ASTNode::ProcessList { processes, .. } = node {
        for process in processes {
            if let ASTNode::Atom { name, args, .. } = process {
                // do alpha conversion
                if name == "=" {
                    let left = &args[0];
                    let left = visit_process(left, holder, from, mem_id);
                    let right = &args[1];
                    let right = visit_process(right, holder, from, mem_id);
                    proc_list.push(left);
                    proc_list.push(right);
                    holder.alpha_connect(left, right);
                    continue;
                }
            }
            proc_list.push(visit_process(process, holder, from, mem_id));
        }
    } else {
        unreachable!("visit_membrane called with non-process node")
    }
    proc_list
}

fn visit_process(
    node: &ASTNode,
    holder: &mut impl ProcessHolder,
    from: Process,
    mem_id: usize,
) -> Process {
    match node {
        ASTNode::Atom { .. } => {
            let id = visit_atom(node, holder, from, mem_id);
            Process::Atom(id)
        }
        ASTNode::Membrane { .. } => {
            let id = visit_membrane(node, holder, from);
            Process::Membrane(id)
        }
        ASTNode::Link {
            name, hyperlink, ..
        } => {
            if *hyperlink {
                todo!("hyperlink")
            } else {
                let link = Link::new(name);
                let id = holder.add_link(link);
                Process::Link(id)
            }
        }
        ASTNode::Context { .. } => {
            todo!("context")
        }
        _ => unreachable!("visit_membrane called with non-process node"),
    }
}

/// Visit an atom node and add it to the holder
///
/// Returns the index of the atom in the holder
fn visit_atom(
    node: &ASTNode,
    holder: &mut impl ProcessHolder,
    from: Process,
    mem_id: usize,
) -> usize {
    if let ASTNode::Atom { name, args, .. } = node {
        let (name, data) = name_dispatch(name);
        let atom = Atom {
            parent: mem_id,
            name: name.clone(),
            args: vec![],
            data,
        };
        let id = holder.add_atom(atom);

        let mut processes = vec![];

        if let Process::Atom(..) = from {
            processes.push(from);
        }

        for arg in args {
            processes.push(visit_process(arg, holder, Process::Atom(id), mem_id));
        }
        let atom = holder.get_atom_mut(id);
        atom.args = processes;
        id
    } else {
        unreachable!("visit_atom called with non-atom node")
    }
}

/// Visit a membrane node and add it to the holder
///
/// Returns the index of the membrane in the holder
fn visit_membrane(node: &ASTNode, holder: &mut impl ProcessHolder, _from: Process) -> usize {
    if let ASTNode::Membrane {
        name,
        process_lists,
        rules,
        ..
    } = node
    {
        let mem = Membrane::new(name.clone());
        let proc = holder.add_membrane(mem);
        let mem = holder.get_membrane_mut(proc);
        // TODO: proxy links
        for process_list in process_lists {
            let process = visit_process_list(process_list, mem, Process::Membrane(proc), proc);
            mem.add_processes(process);
        }
        for rule in rules {
            let rule = visit_rule(rule, Process::Membrane(proc), proc);
            mem.add_rule(rule);
        }
        proc
    } else {
        panic!("visit_membrane called with non-membrane node")
    }
}
