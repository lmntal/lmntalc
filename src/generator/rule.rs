use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use owo_colors::OwoColorize;
use petgraph::{stable_graph::StableDiGraph, visit::IntoNodeReferences};

use crate::{
    data::{
        guard::{GuardNode, GuardSource},
        id::AtomId,
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
pub struct MergeableRuleGenerator<'rule> {
    rules: &'rule [Rule],
    rule_indices: &'rule [usize],
    remove_queue: Vec<LMNtalIR>,
    slot: usize,
    symbol_table: HashMap<AtomId, usize>,
}

#[derive(Debug)]
pub struct RuleIR {
    pub name: String,
    pub cases: Vec<Case>,
    pub pattern: Vec<LMNtalIR>,
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

impl<'rule> MergeableRuleGenerator<'rule> {
    pub fn generate(&mut self) -> RuleIR {
        let pattern = generate_pattern(
            &self.rules[self.rule_indices[0]],
            &mut self.slot,
            &mut self.symbol_table,
            &mut self.remove_queue,
        );
        let mut cases = vec![];

        for rule_idx in self.rule_indices {
            let rule = &self.rules[*rule_idx];
            cases.push(generate_case(
                rule,
                &mut self.slot,
                &mut self.symbol_table,
                self.remove_queue.clone(),
            ));
        }

        RuleIR {
            name: self.rules[self.rule_indices[0]].name.clone(),
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
            0 => zero_incoming.push(*weight),
            1 => {}
            _ => {
                let neighbors: Vec<_> = neighbors
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
    for atom_id in zero_incoming {
        let head = rule.head_atoms();
        let atom = head.get(&atom_id).unwrap();
        inst.push(LMNtalIR::FindAtom {
            id: *slot,
            name: atom.name().to_string(),
            arity: atom.args().count(),
        });
        remove_queue.push(LMNtalIR::RemoveAtom { id: *slot });
        symbol_table.insert(atom_id, *slot);
        *slot += 1;
        for (idx, arg) in atom.args().enumerate() {
            if let RuleLinkArg::Head(arg_id, _) = arg.opposite {
                queue.push_back((atom_id, idx, arg_id));
            }
        }

        while let Some((atom_id, idx, id)) = queue.pop_back() {
            if symbol_table.contains_key(&id) {
                continue;
            }
            let atom = head.get(&id).unwrap();
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

    for atoms in multiple_incoming {
        let mut id_port_list = vec![];
        for (proc, idx) in &atoms {
            id_port_list.push((symbol_table[proc], *idx));
        }
        inst.push(LMNtalIR::AtomEquality {
            id_port_list,
            eq: true,
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
                RuleLinkArg::Head(..) => {}
                RuleLinkArg::None => {
                    if let Some(ty) = arg.opposite_type {
                        condition.push(LMNtalIR::CheckType {
                            id: symbol_table[&atom_id],
                            port: arg.this.index().unwrap(),
                            ty,
                        });
                    }
                    remove_queue.push(LMNtalIR::RemoveAtomAt {
                        id: symbol_table[&atom_id],
                        port: arg.this.index().unwrap(),
                    })
                }
                _ => {
                    if let Some(ty) = arg.opposite_type {
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

    for c in &guard.constraints {
        condition.push(LMNtalIR::CheckValue(transform_guard(c, rule, symbol_table)));
    }

    for (atom_id, guard) in &guard.definitions {
        let defs = rule.definitions();
        let atom = defs.get(atom_id).unwrap();
        if let Some(ty) = atom.type_() {
            definition.push(LMNtalIR::DefineTempVar {
                id: *slot,
                name: atom.name().to_string(),
                ty,
                op: transform_guard(guard, rule, symbol_table),
            });

            symbol_table.insert(*atom_id, *slot);
            *slot += 1;
        }
    }

    let mut adj: HashMap<_, HashMap<_, usize>> = HashMap::new();

    for (i, atom) in rule.body_atoms() {
        body.push(LMNtalIR::CreateAtom {
            id: *slot,
            name: atom.name().to_string(),
            arity: atom.args().count(),
            data: atom.data().clone(),
        });

        symbol_table.insert(i, *slot);
        *slot += 1;

        for (j, arg) in atom.args().enumerate() {
            if let RuleLinkArg::Body(arg, _) = arg.opposite {
                adj.entry(i).or_default().insert(arg, j);
            }
        }
    }

    for (from, list) in &adj {
        for to in list.keys() {
            body.push(LMNtalIR::Link {
                src: ir::VarSource::Body(symbol_table[from], adj[&from][&to]),
                dst: ir::VarSource::Body(symbol_table[to], adj[&to][&from]),
            });
        }
    }

    for atom in rule.all_atoms().values() {
        for arg in atom.args() {
            match (arg.this, arg.opposite) {
                (RuleLinkArg::Head(_, _), RuleLinkArg::Head(_, _))
                | (RuleLinkArg::Body(_, _), RuleLinkArg::Body(_, _)) => {}
                _ => {
                    let link = arg.clone();
                    if let Some(ir) = create_link(&link, symbol_table) {
                        body.push(ir)
                    }
                }
            }
        }
    }

    for ir in remove_queue {
        // body.push(LMNtalIR::RemoveAtom { id: *atom });
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
        (RuleLinkArg::Body(_, _), RuleLinkArg::Head(_, _)) => None, // ignore since it is already handled in the reverse direction
        (RuleLinkArg::Body(_, _), RuleLinkArg::None) => {
            dbg!(link);
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
                dst: VarSource::Definition(symbol_table[&temp_id]),
            })
        }

        (RuleLinkArg::Temp(_, _), _) => None,

        (RuleLinkArg::Body(id, port), RuleLinkArg::Temp(tmp_id, _)) => Some(LMNtalIR::Link {
            src: VarSource::Definition(symbol_table[&tmp_id]),
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
            GuardSource::Definition(def_id) => {
                let defs = rule.definitions();
                let atom = defs.get(def_id).unwrap();
                Operation::Variable {
                    source: ir::VarSource::Definition(symbol_table[def_id]),
                    ty_: atom.type_().unwrap(),
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

/// Construct a directed graph from the pattern, with atoms as nodes and links as edges
///
/// The graph may contain cycles, which will be resolved by `tarjan_scc`
fn construct_graph(rule: &Rule) -> StableDiGraph<AtomId, usize> {
    let mut g = StableDiGraph::default();

    let mut map = HashMap::new();

    for (i, _) in rule.head_atoms() {
        let idx = g.add_node(i);
        map.insert(i, idx);
    }

    for (id, atom) in rule.head_atoms() {
        for (idx, arg) in atom.args().enumerate() {
            if let RuleLinkArg::Head(op_id, _) = arg.opposite {
                let from = *map.get(&id).unwrap();
                let to = *map.get(&op_id).unwrap();
                if id < op_id && !g.contains_edge(from, to) {
                    g.add_edge(from, to, idx);
                } else if id > op_id && !g.contains_edge(to, from) {
                    g.add_edge(to, from, idx);
                }
            }
        }
    }

    g
}
