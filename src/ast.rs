use std::io;

use termtree::Tree;

#[derive(Debug)]
pub enum ASTNode {
    Rule {
        name: String,
        head: Box<ASTNode>,
        guard: Option<Box<ASTNode>>,
        body: Box<ASTNode>,
    },
    ProcessList {
        processes: Vec<ASTNode>,
    },
    Membrane {
        name: String,
        processes: Vec<ASTNode>,
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

pub fn tree(p: &ASTNode) -> io::Result<Tree<String>> {
    let mut root = Tree::new(p.name());
    match p {
        ASTNode::Rule {
            head, guard, body, ..
        } => {
            root.push(tree(head)?);
            if let Some(guard) = guard {
                root.push(tree(guard)?);
            }
            root.push(tree(body)?);
        }
        ASTNode::ProcessList { processes } => {
            for process in processes {
                root.push(tree(process)?);
            }
        }
        ASTNode::Membrane {
            processes, rules, ..
        } => {
            for process in processes {
                root.push(tree(process)?);
            }
            let mut rules_tree = Tree::new("rules".to_owned());
            for rule in rules {
                rules_tree.push(tree(rule)?);
            }
            root.push(rules_tree);
        }
        ASTNode::Atom { args, .. } => {
            if !args.is_empty() {
                let mut args_tree = Tree::new("args".to_owned());
                for arg in args {
                    args_tree.push(tree(arg)?);
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
