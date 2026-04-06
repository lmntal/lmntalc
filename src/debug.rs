use std::io;

use lmntalc_core::syntax::ast::{Atom, Membrane, Process, ProcessList, Rule};
use termtree::Tree;

/// Convert an AST node to a tree for pretty printing.
pub fn tree_root(mem: &Membrane) -> io::Result<Tree<String>> {
    let mut root = Tree::new("Root Membrane".to_owned());
    let mut process_lists_tree = Tree::new("Process Lists".to_owned());
    for process_list in &mem.process_lists {
        process_lists_tree.push(tree_process_list(process_list, "Processes".to_owned())?);
    }
    root.push(process_lists_tree);
    let mut rules_tree = Tree::new("rules".to_owned());
    for rule in &mem.rules {
        rules_tree.push(tree_rule(rule)?);
    }
    root.push(rules_tree);
    Ok(root)
}

fn tree_process_list(process_list: &ProcessList, name: String) -> io::Result<Tree<String>> {
    let mut root = Tree::new(name);
    for process in &process_list.processes {
        root.push(tree_process(process)?);
    }
    Ok(root)
}

fn tree_rule(rule: &Rule) -> io::Result<Tree<String>> {
    let mut root = Tree::new(rule.name.0.clone());
    root.push(tree_process_list(&rule.head, "Head".to_owned())?);
    if let Some(propagation) = &rule.propagation {
        root.push(tree_process_list(propagation, "Propagation".to_owned())?);
    }
    if let Some(guard) = &rule.guard {
        root.push(tree_process_list(guard, "Guard".to_owned())?);
    }
    if let Some(body) = &rule.body {
        root.push(tree_process_list(body, "Body".to_owned())?);
    } else {
        root.push(Tree::new("Empty Body".to_owned()));
    }
    Ok(root)
}

fn tree_process(process: &Process) -> io::Result<Tree<String>> {
    let mut root = Tree::new(process.process_name());
    match process {
        Process::Atom(atom) => {
            if !atom.args.is_empty() {
                let mut args_tree = Tree::new("args".to_owned());
                for arg in &atom.args {
                    args_tree.push(tree_process(arg)?);
                }
                root.push(args_tree);
            }
        }
        Process::Membrane(mem) => {
            let mut process_lists_tree = Tree::new("Process Lists".to_owned());
            for process_list in &mem.process_lists {
                process_lists_tree
                    .push(tree_process_list(process_list, "Process List".to_owned())?);
            }
            root.push(process_lists_tree);
            let mut rules_tree = Tree::new("rules".to_owned());
            for rule in &mem.rules {
                rules_tree.push(tree_rule(rule)?);
            }
            root.push(rules_tree);
        }
        _ => {}
    }
    Ok(root)
}

#[allow(dead_code)]
fn tree_atom(atom: &Atom) -> io::Result<Tree<String>> {
    let mut root = Tree::new(format!("Atom: {}", atom.name.0));
    for arg in &atom.args {
        root.push(tree_process(arg)?);
    }
    Ok(root)
}
