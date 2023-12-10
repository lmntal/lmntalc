use std::io;

use termtree::Tree;

/// An AST node.
#[derive(Debug)]
pub enum ASTNode {
    Rule {
        name: String,
        head: Box<ASTNode>,
        propagation: Option<Box<ASTNode>>,
        guard: Option<Box<ASTNode>>,
        body: Option<Box<ASTNode>>,
    },
    ProcessList {
        processes: Vec<ASTNode>,
    },
    Membrane {
        name: String,
        process_lists: Vec<ASTNode>,
        rules: Vec<ASTNode>,
    },
    Atom {
        name: String,
        args: Vec<ASTNode>,
    },
    Link {
        name: String,
        hyperlink: bool,
    },
    Context {
        name: String,
    },
}

/// Convert an AST node to a tree for pretty printing.
pub fn tree(p: &ASTNode, name: String) -> io::Result<Tree<String>> {
    let mut root = Tree::new(name);
    match p {
        ASTNode::Rule {
            head,
            guard,
            body,
            propagation,
            ..
        } => {
            root.push(tree(head, "Head".to_owned())?);
            if let Some(propagation) = propagation {
                root.push(tree(propagation, "Propagation".to_owned())?);
            }
            if let Some(guard) = guard {
                root.push(tree(guard, "Guard".to_owned())?);
            }
            if let Some(body) = body {
                root.push(tree(body, "Body".to_owned())?);
            } else {
                root.push(Tree::new("Empty Body".to_owned()));
            }
        }
        ASTNode::ProcessList { processes } => {
            for process in processes {
                root.push(tree(process, process.name())?);
            }
        }
        ASTNode::Membrane {
            process_lists,
            rules,
            ..
        } => {
            let mut process_lists_tree = Tree::new("Process Lists".to_owned());
            for process_list in process_lists {
                process_lists_tree.push(tree(process_list, "Process List".to_owned())?);
            }
            root.push(process_lists_tree);
            let mut rules_tree = Tree::new("rules".to_owned());
            for rule in rules {
                rules_tree.push(tree(rule, rule.name())?);
            }
            root.push(rules_tree);
        }
        ASTNode::Atom { args, .. } => {
            if !args.is_empty() {
                let mut args_tree = Tree::new("args".to_owned());
                for arg in args {
                    args_tree.push(tree(arg, arg.name())?);
                }
                root.push(args_tree);
            }
        }
        _ => {}
    }
    Ok(root)
}

impl ASTNode {
    pub fn name(&self) -> String {
        match self {
            ASTNode::Rule { name, .. } => format!("Rule: {}", name),
            ASTNode::ProcessList { .. } => "ProcessList".to_string(),
            ASTNode::Membrane { name, .. } => format!("Membrane: {}", name),
            ASTNode::Atom { name, .. } => format!("Atom: {}", name),
            ASTNode::Link { name, hyperlink } => {
                if *hyperlink {
                    format!("HyperLink: {}", name)
                } else {
                    format!("Link: {}", name)
                }
            }
            ASTNode::Context { name, .. } => format!("Context: {}", name),
        }
    }
}
