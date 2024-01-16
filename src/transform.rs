mod guard;

use crate::{
    ast::{ASTNode, AtomName},
    data::{rule::Rule, *},
};

use self::guard::visit_guard;
use super::data::id::*;

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

    let mut errors = vec![];
    let mut warnings = vec![];

    if let ASTNode::Membrane {
        process_lists,
        rules,
        ..
    } = ast
    {
        // root means void, since the "root membrane" has no parent
        let root = MembraneId::new(0);

        let mut init = Rule::new("_init".to_string(), root);

        let head_id = init.next_membrane_id();
        let head = Membrane::new("_head".to_string(), root);
        init.set_head(head_id);
        init.add_membrane(head_id, head);
        init.set_head_parsed();

        let body_id = init.next_membrane_id();
        let mut body = Membrane::new("_body".to_string(), root);

        for process_list in process_lists {
            let processes = visit_process_list(process_list, &mut init, body_id);
            body.add_processes(processes);
        }

        init.set_body(body_id);
        init.add_membrane(body_id, body);

        program.set_init_rule(init);

        for rule in rules {
            let rule = visit_rule(rule, root);
            program.add_rule(rule);
        }
    } else {
        unreachable!("visit_membrane called with non-membrane node")
    }

    let err_warn = program.solve();

    errors.extend(err_warn.errors);
    warnings.extend(err_warn.warnings);

    TransformResult {
        program,
        warnings,
        errors,
    }
}

/// Visit a rule node and transform it into a `Rule` struct
fn visit_rule(node: &ASTNode, mem_id: MembraneId) -> Rule {
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
        let head_id = rule.next_membrane_id();
        let head_procs = visit_process_list(head, &mut rule, head_id);
        let mut head = Membrane::new("_head".to_string(), mem_id);
        head.add_processes(head_procs);
        rule.set_head(head_id);
        rule.add_membrane(head_id, head);

        if let Some(propagation) = propagation {
            let propagation_id = rule.next_membrane_id();
            let propagation_procs = visit_process_list(propagation, &mut rule, propagation_id);
            let mut propagation = Membrane::new("_propagation".to_string(), mem_id);
            propagation.add_processes(propagation_procs);
            rule.set_propagation(propagation_id);
            rule.add_membrane(propagation_id, propagation);
        }

        rule.set_head_parsed();

        if let Some(guard) = guard {
            rule.guard = visit_guard(&mut rule, guard);
        }

        if let Some(body) = body {
            let body_id = rule.next_membrane_id();
            let body_procs = visit_process_list(body, &mut rule, body_id);
            let mut body = Membrane::new("_body".to_string(), mem_id);
            body.add_processes(body_procs);
            rule.set_body(body_id);
            rule.add_membrane(body_id, body);
        }

        rule
    } else {
        unreachable!("visit_rule called with non-rule node")
    }
}

/// Visit a process list node and transform it into a vector of processes
fn visit_process_list(node: &ASTNode, store: &mut Rule, mem_id: MembraneId) -> Vec<Process> {
    let mut proc_list = vec![];
    if let ASTNode::ProcessList { processes, .. } = node {
        for process in processes {
            visit_process(process, store, &mut proc_list, mem_id);
        }
    } else {
        unreachable!("visit_membrane called with non-process node")
    }
    proc_list
}

fn visit_process(
    node: &ASTNode,
    store: &mut Rule,
    processes: &mut Vec<Process>,
    mem_id: MembraneId,
) {
    match node {
        ASTNode::Atom { .. } => {
            _ = visit_atom(node, store, processes, mem_id);
        }
        ASTNode::Membrane { .. } => {
            let id = visit_membrane(node, store, mem_id);
            processes.push(Process::Membrane(id));
        }
        ASTNode::Link { .. } => {
            panic!("Top level link is not allowed")
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
    rule: &mut Rule,
    processes: &mut Vec<Process>,
    mem_id: MembraneId,
) -> AtomId {
    if let ASTNode::Atom { name, args, .. } = node {
        let (name, data) = match name {
            AtomName::Plain(s) | AtomName::Keyword(s) => (s.to_string(), Data::Empty),
            AtomName::Operator(op) => (op.to_string(), Data::Empty),
            AtomName::Int(i) => ("_int".to_owned(), Data::Int(*i)),
            AtomName::Float(f) => ("_float".to_owned(), Data::Float(*f)),
            AtomName::Char(c) => ("_char".to_owned(), Data::Char(*c)),
        };

        let id = rule.next_atom_id(mem_id);
        processes.push(Process::Atom(id));

        let mut links = vec![];

        for (idx, arg) in args.iter().enumerate() {
            match arg {
                ASTNode::Atom { .. } => {
                    let inner_atom_id = visit_atom(arg, rule, processes, mem_id);
                    let temp_name = rule.temp_link_name();

                    let length = rule.get_atom_arg_len(inner_atom_id);
                    let inner_pair = (inner_atom_id, length);
                    let link_for_inner = Link {
                        name: temp_name.clone(),
                        this: inner_pair,
                        opposite: Some((id, idx)),
                    };
                    rule.append_atom_arg(inner_atom_id, link_for_inner);

                    let link = Link {
                        name: temp_name,
                        this: (id, idx),
                        opposite: Some(inner_pair),
                    };

                    links.push(link);
                }
                ASTNode::Link {
                    name, hyperlink, ..
                } => {
                    if *hyperlink {
                        let name = format!("!{}", name);
                        let hl_id = rule.add_hyperlink(&name);
                        let temp_name = rule.temp_link_name();

                        let length = rule.get_hyperlink_arg_len(hl_id);
                        let this_pair = (id, idx);
                        let hl_pair = (hl_id, length);
                        let link_for_inner = Link {
                            name: temp_name.clone(),
                            this: hl_pair,
                            opposite: Some(this_pair),
                        };
                        rule.append_hyperlink_arg(hl_id, link_for_inner);

                        let link = Link {
                            name: temp_name,
                            this: this_pair,
                            opposite: Some(hl_pair),
                        };

                        links.push(link);
                    } else {
                        let link = Link {
                            name: name.clone(),
                            this: (id, idx),
                            opposite: None,
                        };
                        links.push(link);
                    }
                }
                _ => unimplemented!(),
            }
        }

        let atom = Atom {
            parent: mem_id,
            name: name.clone(),
            args: links,
            data,
        };

        rule.add_atom(id, atom);
        id
    } else {
        unreachable!("visit_atom called with non-atom node")
    }
}

/// Visit a membrane node and add it to the holder
///
/// Returns the index of the membrane in the holder
fn visit_membrane(node: &ASTNode, store: &mut Rule, mem_id: MembraneId) -> MembraneId {
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
            let processes = visit_process_list(process_list, store, mem_id);
            mem.add_processes(processes);
        }

        for rule in rules {
            let rule = visit_rule(rule, mem_id);
            let rule = store.add_rule(rule, mem_id);
            mem.add_rule(rule);
        }

        store.add_membrane(mem_id, mem);

        mem_id
    } else {
        unreachable!("visit_membrane called with non-membrane node")
    }
}
