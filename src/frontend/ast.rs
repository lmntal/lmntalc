use std::{fmt::Display, io};

use super::token::Operator;
use crate::util::Span;
use termtree::Tree;

#[derive(Debug, Clone)]
pub enum AtomName {
    Plain(String),
    Keyword(String),
    Operator(Operator),
    Int(i64),
    Float(f64),
    Char(char),
}

type Spanned<T> = (T, Span);

/// An AST node.
#[derive(Debug)]
pub enum ASTNode {
    Rule {
        /// The name of the rule.
        /// If the rule is anonymous, the `span` will be a dummy span.
        name: Spanned<String>,
        /// The head of the rule.
        head: Box<ASTNode>,
        /// The propagation part of the rule.
        propagation: Option<Box<ASTNode>>,
        /// The guard of the rule.
        guard: Option<Box<ASTNode>>,
        /// The body of the rule.
        body: Option<Box<ASTNode>>,
        /// The whole span of the rule.
        span: Span,
    },
    ProcessList {
        /// all processes in the process list
        processes: Vec<ASTNode>,
        /// The whole span of the process list.
        span: Span,
    },
    Membrane {
        name: Spanned<String>,
        process_lists: Vec<ASTNode>,
        rules: Vec<ASTNode>,
        /// The whole span of the membrane.
        span: Span,
    },
    Atom {
        name: Spanned<AtomName>,
        args: Vec<ASTNode>,
        /// The whole span of the atom.
        ///
        /// If the atom has no arguments or is operator, the span will be the same as the name.
        span: Span,
    },
    Link {
        name: String,
        hyperlink: bool,
        span: Span,
    },
    Context {
        name: String,
        span: Span,
    },
}

impl Display for AtomName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AtomName::Plain(s) => write!(f, "{}", s),
            AtomName::Keyword(s) => write!(f, "{}", s),
            AtomName::Operator(op) => write!(f, "{}", op),
            AtomName::Int(i) => write!(f, "{}", i),
            AtomName::Float(fl) => write!(f, "{}", fl),
            AtomName::Char(c) => write!(f, "{}", c),
        }
    }
}

impl From<Operator> for AtomName {
    fn from(op: Operator) -> Self {
        AtomName::Operator(op)
    }
}

impl ASTNode {
    /// Get the span of the AST node.
    pub fn span(&self) -> Span {
        match self {
            ASTNode::Rule { span, .. } => *span,
            ASTNode::ProcessList { span, .. } => *span,
            ASTNode::Membrane { span, .. } => *span,
            ASTNode::Atom { span, .. } => *span,
            ASTNode::Link { span, .. } => *span,
            ASTNode::Context { span, .. } => *span,
        }
    }
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
        ASTNode::ProcessList { processes, .. } => {
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
            ASTNode::Rule {
                name: (name, ..), ..
            } => format!("Rule: {}", name),
            ASTNode::ProcessList { .. } => "ProcessList".to_string(),
            ASTNode::Membrane {
                name: (name, ..), ..
            } => format!("Membrane: {}", name),
            ASTNode::Atom {
                name: (name, ..), ..
            } => format!("Atom: {}", name),
            ASTNode::Link {
                name, hyperlink, ..
            } => {
                if *hyperlink {
                    format!("Hyperlink: {}", name)
                } else {
                    format!("Link: {}", name)
                }
            }
            ASTNode::Context { name, .. } => format!("Context: {}", name),
        }
    }
}
