use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

use owo_colors::OwoColorize;
use petgraph::{stable_graph::StableDiGraph, visit::IntoNodeReferences};

use crate::{
    data::{
        guard::{GuardNode, GuardSource, ProcessConstraint},
        id::{AtomId, HyperlinkId},
        rule::{Rule, RuleLink, RuleLinkArg},
    },
    ir::{self, LMNtalIR, Operation, VarSource},
};

#[derive(Debug)]
pub struct Case {
    pub condition: Vec<LMNtalIR>,
    pub definition: Vec<LMNtalIR>,
    pub body: Vec<LMNtalIR>,
}

#[derive(Debug)]
pub struct RuleGenerator<'rule> {
    rule: &'rule Rule,
    remove_queue: Vec<LMNtalIR>,
    slot: usize,
    symbol_table: HashMap<AtomId, usize>,
}

#[derive(Debug)]
pub struct RuleIR {
    pub name: String,
    pub pattern: Vec<LMNtalIR>,
    pub cases: Vec<Case>,
}

impl RuleIR {
    /// Compiler hole, do not use
    pub(crate) fn init(self) -> Vec<LMNtalIR> {
        self.cases.into_iter().flat_map(|c| c.body).collect()
    }
}

impl<'rule> RuleGenerator<'rule> {
    pub(crate) fn new(rule: &'rule Rule) -> Self {
        Self {
            rule,
            remove_queue: Vec::new(),
            slot: 0,
            symbol_table: HashMap::new(),
        }
    }

    pub fn generate(&mut self) -> RuleIR {
        let pattern = generate_pattern(
            self.rule,
            &mut self.slot,
            &mut self.symbol_table,
            &mut self.remove_queue,
        );
        let cases = vec![generate_case(
            self.rule,
            &mut self.slot,
            &mut self.symbol_table,
            self.remove_queue.clone(),
        )];

        RuleIR {
            name: self.rule.name.clone(),
            cases,
            pattern,
        }
    }
}

fn generate_pattern(
    rule: &Rule,
    slot: &mut usize,
    symbol_table: &mut HashMap<AtomId, usize>,
    remove_queue: &mut Vec<LMNtalIR>,
) -> Vec<LMNtalIR> {
    let mut inst = vec![];
    // find nodes with 0 incoming edges and nodes with multiple incoming edges
    let mut zero_incoming = vec![];
    let mut multiple_incoming = vec![];
    let pattern = construct_graph(rule);
    for (node, weight) in pattern.node_references() {
        let neighbors: Vec<_> = pattern
            .neighbors_directed(node, petgraph::Direction::Incoming)
            .collect();
        match neighbors.len() {
            0 => {
                if weight.is_atom() {
                    zero_incoming.push(weight.id())
                }
            }
            1 => {}
            _ => {
                let neighbors: HashSet<_> = neighbors
                    .into_iter()
                    .map(|n| {
                        (
                            *pattern.node_weight(n).unwrap(),
                            *pattern.edges_connecting(n, node).next().unwrap().weight(),
                        )
                    })
                    .collect();
                multiple_incoming.push(neighbors);
            }
        }
    }

    let mut queue = VecDeque::new();
    let mut atoms_to_be_different: HashMap<String, Vec<_>> = HashMap::new();
    let mut hyperlinks_to_be_different = HashMap::new();
    let head = rule.head_atoms();
    let hyperlinks = rule.hyperlinks();
    // zero incoming are starting points for every connected component
    for atom_id in zero_incoming {
        let atom = head.get(&atom_id).unwrap();
        inst.push(LMNtalIR::FindAtom {
            id: *slot,
            name: atom.name().to_string(),
            arity: atom.args().count(),
        });
        // atoms between different connected components should be different
        if let Some(set) = atoms_to_be_different.get_mut(atom.name()) {
            let mut id_list = vec![*slot];
            for id in set.iter() {
                id_list.push(*id);
            }
            inst.push(LMNtalIR::AtomEquality {
                id_list,
                eq: false,
                hyperlinks: false,
            });
        } else {
            atoms_to_be_different.insert(atom.name().to_string(), vec![*slot]);
        }
        remove_queue.push(LMNtalIR::RemoveAtom { id: *slot });
        symbol_table.insert(atom_id, *slot);
        *slot += 1;
        for (idx, arg) in atom.args().enumerate() {
            if let RuleLinkArg::Head(arg_id, _) = arg.opposite {
                if hyperlinks.contains_key(&arg_id) {
                    inst.push(LMNtalIR::GetHyperlinkAtPort {
                        id: *slot,
                        from: symbol_table[&atom_id],
                        port: idx,
                    });
                    hyperlinks_to_be_different.insert(arg_id, *slot);
                    symbol_table.insert(arg_id, *slot);
                    *slot += 1;
                    remove_queue.push(LMNtalIR::RemoveFromHyperlink {
                        atom: VarSource::Head(symbol_table[&atom_id], idx),
                        hyperlink: VarSource::Head(symbol_table[&arg_id], 0),
                    });
                } else {
                    // ignore hyperlinks, they are handled separately
                    queue.push_back((atom_id, idx, arg_id));
                }
            }
        }

        // Traverse the graph and create atoms
        while let Some((atom_id, idx, id)) = queue.pop_back() {
            if symbol_table.contains_key(&id) {
                continue;
            }
            if let Some(atom) = head.get(&id) {
                inst.push(LMNtalIR::GetAtomAtPort {
                    id: *slot,
                    from: symbol_table[&atom_id],
                    port: idx,
                    name: atom.name().to_string(),
                    arity: atom.args().count(),
                });
                remove_queue.push(LMNtalIR::RemoveAtom { id: *slot });
                symbol_table.insert(id, *slot);
                *slot += 1;
                for (idx, arg) in atom.args().enumerate() {
                    if let RuleLinkArg::Head(arg_id, _) = arg.opposite {
                        queue.push_back((id, idx, arg_id));
                    }
                }
            }
        }
    }

    // Check atoms with multiple incoming edges
    for atoms in multiple_incoming {
        let mut id_port_list = vec![];
        for (proc, idx) in &atoms {
            id_port_list.push((symbol_table[&proc.id()], *idx));
        }
        inst.push(LMNtalIR::AtomEqualityIdPort {
            id_port_list,
            eq: true, // they should be equal
        });
    }

    if hyperlinks_to_be_different.len() > 1 {
        inst.push(LMNtalIR::AtomEquality {
            id_list: hyperlinks_to_be_different.values().copied().collect(),
            eq: false,
            hyperlinks: true,
        });
    }

    inst
}

fn generate_case(
    rule: &Rule,
    slot: &mut usize,
    symbol_table: &mut HashMap<AtomId, usize>,
    mut remove_queue: Vec<LMNtalIR>,
) -> Case {
    let mut condition = vec![];
    let mut definition = vec![];
    let mut body = vec![];
    let guard = &rule.guard;

    for (atom_id, atom) in rule.head_atoms() {
        for arg in atom.args() {
            match arg.opposite {
                RuleLinkArg::None => {
                    if let Some(ty) = arg.opposite_type {
                        if ty != ProcessConstraint::Hyperlink {
                            condition.push(LMNtalIR::CheckType {
                                id: symbol_table[&atom_id],
                                port: arg.this.index().unwrap(),
                                ty,
                            });
                        }
                    }
                    remove_queue.push(LMNtalIR::RemoveAtomAt {
                        id: symbol_table[&atom_id],
                        port: arg.this.index().unwrap(),
                    })
                }
                _ => {
                    if let Some(ty) = arg.opposite_type {
                        // hyperlinks are checked at pattern matching
                        if ty != ProcessConstraint::Hyperlink {
                            condition.push(LMNtalIR::CheckType {
                                id: symbol_table[&atom_id],
                                port: arg.this.index().unwrap(),
                                ty,
                            });
                        }
                    }
                }
            }
        }
    }

    for c in &guard.constraints {
        if let GuardNode::Constraint(_, _) = c {
        } else {
            condition.push(LMNtalIR::CheckValue(transform_guard(c, rule, symbol_table)));
        }
    }

    for (var_id, var) in &guard.definitions {
        definition.push(LMNtalIR::DefineTempVar {
            id: (*var_id).into(),
            name: var.name.to_string(),
            ty: var.ty.unwrap(),
            op: transform_guard(&var.node, rule, symbol_table),
        });
    }

    definition.sort_by(|a, b| match (a, b) {
        (LMNtalIR::DefineTempVar { id: a, .. }, LMNtalIR::DefineTempVar { id: b, .. }) => a.cmp(b),
        _ => unreachable!(),
    });

    for (atom_id, atom) in rule.var_atoms() {
        body.push(LMNtalIR::CreateAtom {
            id: *slot,
            name: atom.name().to_string(),
            arity: 1,
            data: atom.data().clone(),
        });
        symbol_table.insert(atom_id, *slot);
        *slot += 1;
    }

    let mut link_queue = vec![];
    let mut skip_fuse = HashSet::new();

    for (atom_id, atom) in rule.body_atoms() {
        if atom.name() == "><" && atom.args().count() == 2 {
            let mut args = atom.args();
            let arg1 = args.next().unwrap();
            let arg2 = args.next().unwrap();
            if let RuleLinkArg::Head(op1, _) = arg1.opposite {
                if let RuleLinkArg::Head(op2, _) = arg2.opposite {
                    // fuse hyperlinks after all links are created, in case the atoms linked to the hyperlinks that are being fused
                    remove_queue.push(LMNtalIR::FuseHyperlink {
                        into: VarSource::Head(symbol_table[&op1], 0),
                        from: VarSource::Head(symbol_table[&op2], 0),
                    });
                    skip_fuse.insert(atom_id);
                    continue;
                }
            }
        }

        body.push(LMNtalIR::CreateAtom {
            id: *slot,
            name: atom.name().to_string(),
            arity: atom.args().count(),
            data: atom.data().clone(),
        });
        symbol_table.insert(atom_id, *slot);
        *slot += 1;
    }

    for (hl_id, hl) in rule.hyperlinks() {
        if !hl.first_in_head() {
            // only create hyperlinks that are not in the head
            body.push(LMNtalIR::CreateHyperlink {
                id: *slot,
                name: hl.name().to_string(),
            });
            symbol_table.insert(hl_id, *slot);
            *slot += 1;
        }
    }

    for (this_id, atom) in rule.body_atoms() {
        if skip_fuse.contains(&this_id) {
            continue;
        }
        for (this_port, arg) in atom.args().enumerate() {
            match arg.opposite {
                RuleLinkArg::Body(op, port) => {
                    link_queue.push({
                        LMNtalIR::Link {
                            src: VarSource::Body(symbol_table[&this_id], this_port),
                            dst: VarSource::Body(symbol_table[&op], port),
                        }
                    });
                }
                RuleLinkArg::Head(op, port) => {
                    if rule.hyperlinks().contains_key(&op) {
                        link_queue.push({
                            LMNtalIR::LinkToHyperlink {
                                atom: VarSource::Body(symbol_table[&this_id], this_port),
                                hyperlink: VarSource::Body(symbol_table[&op], port),
                            }
                        });
                    }
                }
                _ => {}
            }
        }
    }

    for atom in rule.all_atoms().values() {
        for arg in atom.args() {
            match (arg.this, arg.opposite) {
                (RuleLinkArg::Head(_, _), RuleLinkArg::Head(_, _))
                | (RuleLinkArg::Body(_, _), RuleLinkArg::Body(_, _)) => {}
                (RuleLinkArg::Body(body_id, body_port), RuleLinkArg::Head(head_id, head_port)) => {
                    if rule
                        .all_atoms()
                        .get(&head_id)
                        .unwrap()
                        .args()
                        .nth(head_port)
                        .unwrap()
                        .opposite_type
                        .is_none()
                    {
                        continue;
                    }
                    // case that clone is needed
                    body.push(LMNtalIR::CloneAtom {
                        id: *slot,
                        from_id: symbol_table[&head_id],
                        from_port: head_port,
                    });
                    // do not update symbol table, since the atom won't be used again
                    *slot += 1;
                    link_queue.push(LMNtalIR::Link {
                        src: VarSource::Body(symbol_table[&body_id], body_port),
                        dst: VarSource::Body(*slot - 1, 0),
                    });
                }
                _ => {
                    let link = arg.clone();
                    if let Some(ir) = create_link(&link, symbol_table) {
                        link_queue.push(ir)
                    }
                }
            }
        }
    }

    let mut map = HashSet::new();

    for ir in link_queue {
        if let LMNtalIR::Link { src, dst } = ir {
            if map.contains(&(src, dst)) {
                continue;
            } else {
                map.insert((dst, src));
                body.push(ir);
            }
        } else {
            body.push(ir);
        }
    }

    for ir in remove_queue {
        body.push(ir);
    }

    Case {
        condition,
        definition,
        body,
    }
}

fn create_link(link: &RuleLink, symbol_table: &HashMap<AtomId, usize>) -> Option<LMNtalIR> {
    match (link.this, link.opposite) {
        (RuleLinkArg::None, _) => unreachable!(),
        (RuleLinkArg::Head(_, _), RuleLinkArg::None) => {
            if link.opposite_type.is_some() || link.opposite.is_body() {
                None
            } else {
                panic!("link {} is not constrained", link.name);
            }
        }
        (RuleLinkArg::Head(src, src_port), RuleLinkArg::Body(dst, dst_port)) => {
            Some(LMNtalIR::Relink {
                src: symbol_table[&src],
                src_port,
                dst: VarSource::Body(symbol_table[&dst], dst_port),
            })
        }
        (RuleLinkArg::Body(_, _), RuleLinkArg::Head(_, _)) => None, // ignore since it is already handled outside
        (RuleLinkArg::Body(_, _), RuleLinkArg::None) => {
            panic!("link {} is not constrained", link.name);
        }
        (RuleLinkArg::Head(p1, idx1), RuleLinkArg::Head(p2, idx2)) => Some(LMNtalIR::Link {
            src: VarSource::Head(symbol_table[&p1], idx1),
            dst: VarSource::Head(symbol_table[&p2], idx2),
        }),
        (RuleLinkArg::Body(p1, idx1), RuleLinkArg::Body(p2, idx2)) => Some(LMNtalIR::Link {
            src: VarSource::Body(symbol_table[&p1], idx1),
            dst: VarSource::Body(symbol_table[&p2], idx2),
        }),
        (RuleLinkArg::Head(head_id, port), RuleLinkArg::Temp(temp_id, _)) => {
            Some(LMNtalIR::Relink {
                src: symbol_table[&head_id],
                src_port: port,
                dst: VarSource::Variable(symbol_table[&temp_id]),
            })
        }

        (RuleLinkArg::Temp(_, _), _) => None,

        (RuleLinkArg::Body(id, port), RuleLinkArg::Temp(tmp_id, _)) => Some(LMNtalIR::Link {
            src: VarSource::Variable(symbol_table[&tmp_id]),
            dst: VarSource::Body(symbol_table[&id], port),
        }),
    }
}

fn transform_guard(
    guard: &GuardNode,
    rule: &Rule,
    symbol_table: &HashMap<AtomId, usize>,
) -> Operation {
    match guard {
        GuardNode::Var(p) => match p {
            GuardSource::AtPortOfAtom(atom_id, port) => {
                if rule.is_in_head(atom_id.parent()) {
                    let atom = rule.all_atoms().get(atom_id).unwrap();
                    let link = atom.args().nth(*port).unwrap();
                    Operation::Variable {
                        source: ir::VarSource::Head(symbol_table[&atom_id], *port),
                        ty_: link.opposite_type.unwrap(),
                    }
                } else {
                    unreachable!()
                }
            }
            GuardSource::Variable(def_id) => {
                let def = rule.guard.definitions.get(def_id).unwrap();
                Operation::Variable {
                    source: ir::VarSource::Variable((*def_id).into()),
                    ty_: def.ty.unwrap(),
                }
            }
            GuardSource::Placeholder(_) => unreachable!(),
        },
        GuardNode::Int(i) => Operation::Literal(ir::Literal::Int(*i)),
        GuardNode::Float(f) => Operation::Literal(ir::Literal::Float(*f)),
        GuardNode::Binary(op, lhs, rhs) => {
            let lhs = transform_guard(lhs, rule, symbol_table);
            let rhs = transform_guard(rhs, rule, symbol_table);
            Operation::BinaryOP {
                op: op.into(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }
        }
        GuardNode::Function(sig, args) => {
            let mut ir_args = vec![];
            for arg in args {
                ir_args.push(transform_guard(arg, rule, symbol_table));
            }
            Operation::FunctionCall {
                name: sig.name.to_owned(),
                args: ir_args,
                ty_: sig.ret,
            }
        }
        _ => unreachable!(),
    }
}

impl Display for RuleIR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{}\t{}",
            "Rule".underline().bold().blue(),
            self.name.bold()
        )?;
        writeln!(f, "{}", "pattern".underline().bright_blue())?;
        for ir in &self.pattern {
            writeln!(f, "\t{}", ir)?;
        }
        for case in &self.cases {
            writeln!(f, "{}", "case".underline().bright_blue())?;
            for ir in &case.condition {
                writeln!(f, " \t{}", ir)?;
            }
            for ir in &case.definition {
                writeln!(f, " \t{}", ir)?;
            }
            for ir in &case.body {
                writeln!(f, "\t{}", ir)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Node {
    Atom(AtomId),
    Hyperlink(HyperlinkId),
}

impl Node {
    fn is_atom(&self) -> bool {
        matches!(self, Node::Atom(_))
    }

    fn id(&self) -> AtomId {
        match self {
            Node::Atom(id) => *id,
            Node::Hyperlink(id) => *id,
        }
    }
}

/// Construct a directed graph from the pattern, with atoms as nodes and links as edges
///
/// The graph may contain cycles, which will be resolved by `tarjan_scc`
fn construct_graph(rule: &Rule) -> StableDiGraph<Node, usize> {
    let mut g = StableDiGraph::default();

    let mut map = HashMap::new();

    for (i, _) in rule.head_atoms() {
        let idx = g.add_node(Node::Atom(i));
        map.insert(i, idx);
    }

    for (i, hl) in rule.hyperlinks() {
        if hl.first_in_head() {
            let idx = g.add_node(Node::Hyperlink(i));
            map.insert(i, idx);
        }
    }

    for (id, atom) in rule.head_atoms() {
        for (idx, arg) in atom.args().enumerate() {
            if let RuleLinkArg::Head(op_id, _) = arg.opposite {
                let from = *map.get(&id).unwrap();
                if let Some(to) = map.get(&op_id) {
                    // maybe a hyperlink
                    if id < op_id && !g.contains_edge(from, *to) {
                        g.add_edge(from, *to, idx);
                    } else if id > op_id && !g.contains_edge(*to, from) {
                        g.add_edge(*to, from, idx);
                    }
                }
            }
        }
    }

    g
}
