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

#[derive(Debug)]
pub struct Rule {
    /// The name of the rule.
    /// If the rule is anonymous, the `span` will be a dummy span.
    pub name: Spanned<String>,
    /// The head of the rule.
    pub head: ProcessList,
    /// The propagation part of the rule.
    pub propagation: Option<ProcessList>,
    /// The guard of the rule.
    pub guard: Option<ProcessList>,
    /// The body of the rule.
    pub body: Option<ProcessList>,
    /// The whole span of the rule.
    pub span: Span,
}

#[derive(Debug)]
pub struct Atom {
    pub name: Spanned<AtomName>,
    pub args: Vec<Process>,
    /// The whole span of the atom.
    ///
    /// If the atom has no arguments or is operator, the span will be the same as the name.
    pub span: Span,
}

#[derive(Debug)]
pub struct Membrane {
    pub name: Spanned<String>,
    pub process_lists: Vec<ProcessList>,
    pub rules: Vec<Rule>,
    /// The whole span of the membrane.
    pub span: Span,
}

#[derive(Debug)]
pub struct ProcessList {
    /// all processes in the process list
    pub processes: Vec<Process>,
    /// The whole span of the process list.
    pub span: Span,
}

#[derive(Debug)]
pub struct Link {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Hyperlink {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct Context {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub enum Process {
    Atom(Atom),
    Membrane(Membrane),
    Link(Link),
    Hyperlink(Hyperlink),
    Context(Context),
}

macro_rules! impl_process {
    (
        $(
            $variant:ident($ty:ty),
        )*
    ) => {
        $(
            impl From<$ty> for Process {
                fn from(p: $ty) -> Self {
                    Process::$variant(p)
                }
            }
        )*
    };
}

impl_process! {
    Atom(Atom),
    Membrane(Membrane),
    Link(Link),
    Hyperlink(Hyperlink),
    Context(Context),
}

impl Process {
    pub fn process_name(&self) -> String {
        match self {
            Process::Atom(atom) => format!("Atom: {}", atom.name.0),
            Process::Membrane(membrane) => format!("Membrane: {}", membrane.name.0),
            Process::Link(link) => format!("Link: {}", link.name),
            Process::Hyperlink(hyperlink) => format!("Hyperlink: {}", hyperlink.name),
            Process::Context(context) => format!("Context: {}", context.name),
        }
    }
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
