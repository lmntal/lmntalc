use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use crate::{
    lowering::{SolveResult, TransformError},
    text::Span,
};

use super::guard::{GuardNode, GuardSource, ProcessConstraint, VariableId};
use super::{guard::Guard, Atom, Data, Link, Membrane};

use super::id::*;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RuleAtom {
    parent: MembraneId,
    name: String,
    data: Data,
    span: Span,
    type_: Option<ProcessConstraint>,
    args: Vec<RuleLink>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct RuleHyperlink {
    name: String,
    span: Span,
    first_in_head: bool,
    args: Vec<RuleLink>,
}

#[derive(Debug, Default, Clone)]
pub struct Rule {
    /// Parent membrane
    pub parent: MembraneId,
    /// Name of the rule
    pub name: String,
    /// Whole-rule span for diagnostics
    pub span: Span,
    /// Head
    pub head: MembraneId,
    /// Propagation, with order preserved
    pub propagation: Option<MembraneId>,
    /// Guards, with order preserved
    pub guard: Guard,
    /// Body
    pub body: MembraneId,

    head_parsed: bool,

    head_membranes: HashSet<MembraneId>,
    body_membranes: HashSet<MembraneId>,

    membranes: HashMap<MembraneId, Membrane>,
    atoms: HashMap<AtomId, RuleAtom>,
    hyperlinks: HashMap<HyperlinkId, RuleHyperlink>,
    var_atoms: HashMap<VariableId, Vec<AtomId>>,
    vars: HashMap<AtomId, RuleAtom>,

    id_generator: IdGenerator,
}

/// Argument of a link in a rule
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RuleLinkArg {
    /// Not linked to any atom yet
    None,
    /// Appears in the head of the rule,
    /// with the position of the link in the `Process`
    Head(AtomId, usize),
    /// Temporary variable, like `X` in `X = Y + 1`
    Temp(AtomId, usize),
    /// Appears in the body of the rule,
    /// with the position of the link in the `Process`
    Body(AtomId, usize),
}

#[derive(Debug, Clone)]
pub struct RuleLink {
    pub name: String,
    pub span: Span,
    pub this: RuleLinkArg,
    pub opposite_type: Option<ProcessConstraint>,
    pub opposite: RuleLinkArg,
}

impl RuleHyperlink {
    pub(crate) fn first_in_head(&self) -> bool {
        self.first_in_head
    }
}

impl RuleAtom {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> impl Iterator<Item = &RuleLink> {
        self.args.iter()
    }

    pub fn data(&self) -> &Data {
        &self.data
    }

    pub fn type_(&self) -> Option<ProcessConstraint> {
        self.type_
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl RuleHyperlink {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn args(&self) -> impl Iterator<Item = &RuleLink> {
        self.args.iter()
    }
}

impl RuleLink {
    fn from(link: &Link, this_in_head: bool, op_in_head: bool) -> Self {
        Self {
            name: link.name.clone(),
            span: link.span,
            this: if this_in_head {
                RuleLinkArg::Head(link.this.0, link.this.1)
            } else {
                RuleLinkArg::Body(link.this.0, link.this.1)
            },
            opposite_type: None,
            opposite: match &link.opposite {
                Some(opposite) => {
                    if op_in_head {
                        RuleLinkArg::Head(opposite.0, opposite.1)
                    } else {
                        RuleLinkArg::Body(opposite.0, opposite.1)
                    }
                }
                None => RuleLinkArg::None,
            },
        }
    }
}

#[allow(dead_code)]
impl RuleLinkArg {
    pub(crate) fn id_index(&self) -> Option<(AtomId, usize)> {
        match self {
            Self::Head(id, index) | Self::Temp(id, index) | Self::Body(id, index) => {
                Some((*id, *index))
            }
            RuleLinkArg::None => None,
        }
    }

    pub(crate) fn atom_id(&self) -> Option<AtomId> {
        match self {
            Self::Head(id, _) | Self::Temp(id, _) | Self::Body(id, _) => Some(*id),
            RuleLinkArg::None => None,
        }
    }

    pub(crate) fn index(&self) -> Option<usize> {
        match self {
            Self::Head(_, index) | Self::Temp(_, index) | Self::Body(_, index) => Some(*index),
            RuleLinkArg::None => None,
        }
    }

    pub(crate) fn is_head(&self) -> bool {
        matches!(self, RuleLinkArg::Head(_, _))
    }

    pub(crate) fn is_body(&self) -> bool {
        matches!(self, RuleLinkArg::Body(_, _))
    }
}

impl Display for RuleLinkArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Head(id, index) => write!(f, "Head({}, {})", id, index),
            Self::Temp(id, index) => write!(f, "Temp({}, {})", id, index),
            Self::Body(id, index) => write!(f, "Body({}, {})", id, index),
            RuleLinkArg::None => write!(f, "None"),
        }
    }
}

impl Rule {
    pub(crate) fn next_membrane_id(&mut self) -> MembraneId {
        self.id_generator.next_membrane_id()
    }

    pub(crate) fn next_atom_id(&mut self, parent: MembraneId) -> AtomId {
        self.id_generator.next_atom_id(parent)
    }

    pub(crate) fn temp_link_name(&mut self) -> String {
        let id = self.id_generator.next_link_id();
        format!("~temp{}", id)
    }

    pub(crate) fn add_membrane(&mut self, id: MembraneId, membrane: Membrane) {
        if self.head_membranes.contains(&membrane.parent) {
            self.head_membranes.insert(id);
        } else if self.body_membranes.contains(&membrane.parent) {
            self.body_membranes.insert(id);
        }
        self.membranes.insert(id, membrane);
    }

    pub(crate) fn add_atom(&mut self, id: AtomId, atom: Atom) {
        let atom = RuleAtom {
            parent: atom.parent,
            name: atom.name,
            span: atom.span,
            type_: match &atom.data {
                Data::Int(_) => Some(ProcessConstraint::Int),
                Data::Float(_) => Some(ProcessConstraint::Float),
                Data::String(_) => Some(ProcessConstraint::String),
                _ => None,
            },
            data: atom.data,
            args: atom
                .args
                .iter()
                .map(|link| {
                    let head = if let Some((id, _)) = link.opposite {
                        if let Some(hl) = self.hyperlinks.get(&id) {
                            hl.first_in_head
                        } else {
                            !self.head_parsed
                        }
                    } else {
                        !self.head_parsed
                    };
                    RuleLink::from(link, !self.head_parsed, head)
                })
                .collect(),
        };
        self.atoms.insert(id, atom);
    }

    pub(crate) fn add_hyperlink(&mut self, name: &str, span: Span) -> super::HyperlinkId {
        for (id, link_) in &self.hyperlinks {
            if link_.name == name {
                return *id;
            }
        }

        let id = self.id_generator.next_hyperlink_id();
        let hyperlink = RuleHyperlink {
            name: name.to_owned(),
            span,
            first_in_head: !self.head_parsed,
            args: vec![],
        };
        self.hyperlinks.insert(id, hyperlink);
        id
    }

    pub(crate) fn get_atom_arg_len(&self, id: AtomId) -> usize {
        self.atoms.get(&id).map_or(0, |atom| atom.args.len())
    }

    pub(crate) fn get_hyperlink_arg_len(&self, id: HyperlinkId) -> usize {
        self.hyperlinks.get(&id).map_or(0, |link| link.args.len())
    }

    pub(crate) fn append_atom_arg(&mut self, id: AtomId, link: Link) {
        self.atoms
            .get_mut(&id)
            .expect("atom must exist")
            .args
            .push(RuleLink::from(&link, !self.head_parsed, !self.head_parsed));
    }

    pub(crate) fn append_hyperlink_arg(&mut self, id: HyperlinkId, link: Link) {
        self.hyperlinks
            .get_mut(&id)
            .expect("hyperlink must exist")
            .args
            .push(RuleLink::from(&link, !self.head_parsed, !self.head_parsed));
    }
}

/// Helper struct for solving rules
#[derive(Debug, Clone, Copy)]
struct LinkInfo {
    #[allow(dead_code)]
    this: RuleLinkArg,
    this_type: Option<ProcessConstraint>,
    opposite: RuleLinkArg,
    span: Span,
}

impl Rule {
    pub(crate) fn new(name: String, mem_id: MembraneId, span: Span) -> Self {
        Self {
            name,
            parent: mem_id,
            span,
            ..Default::default()
        }
    }

    pub(crate) fn all_atoms(&self) -> &HashMap<AtomId, RuleAtom> {
        &self.atoms
    }

    pub(crate) fn get_atom(&self, id: AtomId) -> Option<&RuleAtom> {
        self.atoms.get(&id)
    }

    pub(crate) fn contains_hyperlink(&self, id: HyperlinkId) -> bool {
        self.hyperlinks.contains_key(&id)
    }

    pub(crate) fn head_atoms(&self) -> impl Iterator<Item = (AtomId, &RuleAtom)> + '_ {
        self.membranes[&self.head]
            .atoms
            .iter()
            .map(move |id| (*id, &self.atoms[id]))
    }

    pub(crate) fn var_atoms(&self) -> impl Iterator<Item = (AtomId, &RuleAtom)> + '_ {
        self.vars.iter().map(|(id, atom)| (*id, atom))
    }

    pub(crate) fn body_atoms(&self) -> impl Iterator<Item = (AtomId, &RuleAtom)> + '_ {
        self.membranes[&self.body]
            .atoms
            .iter()
            .map(move |id| (*id, &self.atoms[id]))
    }

    pub(crate) fn hyperlinks(&self) -> impl Iterator<Item = (HyperlinkId, &RuleHyperlink)> + '_ {
        self.hyperlinks.iter().map(|(id, link)| (*id, link))
    }

    pub(crate) fn is_in_head(&self, id: MembraneId) -> bool {
        self.head_membranes.contains(&id)
    }

    pub(crate) fn set_head_parsed(&mut self) {
        self.head_parsed = true;
    }

    pub(crate) fn set_head(&mut self, head: MembraneId) {
        self.head = head;
        self.head_membranes.insert(head);
    }

    pub(crate) fn set_propagation(&mut self, propagation: MembraneId) {
        self.propagation = Some(propagation);
        self.head_membranes.insert(propagation);
    }

    pub(crate) fn set_body(&mut self, body: MembraneId) {
        self.body = body;
        self.body_membranes.insert(body);
    }

    pub(super) fn solve(&mut self) -> SolveResult {
        let mut free = HashMap::new();
        let mut result = SolveResult::default();

        self.apply_unification_pass(self.head);
        result.combine(self.resolve_head_links(&mut free));

        if let Some(propagation) = self.propagation {
            result.combine(self.resolve_membrane_links(propagation, &mut free));
        }

        result.combine(self.resolve_guard_types(&mut free));
        result.combine(self.resolve_body_links(&mut free));
        self.apply_unification_pass(self.body);

        for (link, info) in free {
            result.errors.push(TransformError::UnconstrainedLink {
                link,
                span: info.span,
            });
        }

        result
    }

    fn resolve_head_links(&mut self, free_links: &mut HashMap<String, LinkInfo>) -> SolveResult {
        self.resolve_membrane_links(self.head, free_links)
    }

    fn resolve_body_links(&mut self, free_links: &mut HashMap<String, LinkInfo>) -> SolveResult {
        self.resolve_membrane_links(self.body, free_links)
    }

    fn resolve_membrane_links(
        &mut self,
        membrane_id: MembraneId,
        free_links: &mut HashMap<String, LinkInfo>,
    ) -> SolveResult {
        let child_membranes: Vec<_> = self.membranes[&membrane_id]
            .membranes
            .iter()
            .copied()
            .collect();
        let atom_ids: Vec<_> = self.membranes[&membrane_id].atoms.iter().copied().collect();

        let mut result = SolveResult::default();
        for mem in child_membranes {
            result.combine(self.resolve_membrane_links(mem, free_links));
        }

        let mut connected = HashSet::new();
        let mut updates = vec![];

        for atom_id in atom_ids {
            let atom = self.atoms.get_mut(&atom_id).expect("atom must exist");
            for link in &mut atom.args {
                if let Some(id) = link.opposite.atom_id() {
                    if self.hyperlinks.contains_key(&id) {
                        link.opposite_type = Some(ProcessConstraint::Hyperlink);
                    }
                    continue;
                }

                if let Some(var_id) = self.guard.defined(&link.name) {
                    match Self::temp_atom_name_for_guard(&self.guard, self.span, &link.name, var_id)
                    {
                        Ok(name) => {
                            let id = self.id_generator.next_atom_id(self.parent);
                            let atom = RuleAtom {
                                parent: self.parent,
                                name,
                                data: Data::Variable(var_id),
                                span: link.span,
                                type_: None,
                                args: vec![],
                            };
                            self.var_atoms.entry(var_id).or_default().push(id);
                            self.vars.insert(id, atom);
                            link.opposite = RuleLinkArg::Temp(id, 0);
                        }
                        Err(error) => {
                            result.errors.push(error);
                            continue;
                        }
                    }
                }

                if connected.contains(&link.name) {
                    result.errors.push(TransformError::LinkTooManyOccurrence {
                        link: link.name.clone(),
                        span: link.span,
                    });
                    continue;
                }

                let new_info = LinkInfo {
                    this: link.opposite,
                    this_type: link.opposite_type,
                    opposite: link.this,
                    span: link.span,
                };

                if let Some(other) = free_links.insert(link.name.clone(), new_info) {
                    updates.push((other.opposite, link.this));
                    connected.insert(link.name.clone());
                }
            }
        }

        for name in connected {
            free_links.remove(&name);
        }

        for (from, to) in updates {
            if let Some((id, idx)) = from.id_index() {
                let atom = self.atoms.get_mut(&id).expect("atom must exist");
                atom.args[idx].opposite = to;
            }
            if let Some((id, idx)) = to.id_index() {
                let atom = self.atoms.get_mut(&id).expect("atom must exist");
                atom.args[idx].opposite = from;
            }
        }

        result
    }

    fn resolve_guard_types(&mut self, free_links: &mut HashMap<String, LinkInfo>) -> SolveResult {
        let mut guard = self.guard.clone();
        let mut result = SolveResult::default();

        for constraint in guard.constraints.iter_mut() {
            match constraint {
                GuardNode::Constraint(func, vars) => {
                    'var: for var in vars.iter_mut() {
                        if let GuardSource::Placeholder(name) = var {
                            if let Some(link) = free_links.get_mut(name) {
                                if let Some((id, idx)) = link.opposite.id_index() {
                                    let atom = self.atoms.get_mut(&id).expect("atom must exist");
                                    atom.args[idx].opposite_type = Some(*func);
                                    link.this_type = Some(*func);
                                    continue 'var;
                                }
                            }
                            result.errors.push(TransformError::UnconstrainedLink {
                                link: name.clone(),
                                span: self.span,
                            });
                        }
                    }
                }
                GuardNode::Binary(op, lhs, rhs) => {
                    let expected = (*op).into();
                    if let Err(error) =
                        self.substitute_link_in_guard_with_type(lhs, &expected, free_links)
                    {
                        result.errors.push(error);
                    }
                    if let Err(error) =
                        self.substitute_link_in_guard_with_type(rhs, &expected, free_links)
                    {
                        result.errors.push(error);
                    }
                }
                _ => {}
            }
        }

        for var in guard.definitions.values_mut() {
            match &mut var.node {
                GuardNode::Var(var_source) => {
                    if let GuardSource::Placeholder(name) = var_source {
                        if let Some(link) = free_links.get_mut(name) {
                            if let Some((id, idx)) = link.opposite.id_index() {
                                *var_source = GuardSource::AtPortOfAtom(id, idx);
                            }
                            continue;
                        }
                        result.errors.push(TransformError::UnconstrainedLink {
                            link: name.clone(),
                            span: self.span,
                        });
                    }
                }
                GuardNode::Binary(op, lhs, rhs) => {
                    let expected = (*op).into();
                    if let Err(error) =
                        self.substitute_link_in_guard_with_type(lhs, &expected, free_links)
                    {
                        result.errors.push(error);
                    }
                    if let Err(error) =
                        self.substitute_link_in_guard_with_type(rhs, &expected, free_links)
                    {
                        result.errors.push(error);
                    }
                    var.ty = Some(expected);
                }
                GuardNode::Function(sig, args) => {
                    var.ty = Some(sig.ret);
                    for (arg, arg_type) in args.iter_mut().zip(sig.args.iter().cycle()) {
                        if let Err(error) =
                            self.substitute_link_in_guard_with_type(arg, arg_type, free_links)
                        {
                            result.errors.push(error);
                        }
                    }
                }
                GuardNode::Constraint(..) => {}
                GuardNode::Int(_) => {
                    var.ty = Some(ProcessConstraint::Int);
                }
                GuardNode::Float(_) => {
                    var.ty = Some(ProcessConstraint::Float);
                }
            }
        }

        self.guard = guard;
        result
    }

    fn substitute_link_in_guard_with_type(
        &mut self,
        node: &mut GuardNode,
        type_: &ProcessConstraint,
        free_links: &mut HashMap<String, LinkInfo>,
    ) -> Result<ProcessConstraint, TransformError> {
        match node {
            GuardNode::Var(var) => {
                let mut substitute = None;
                match var {
                    GuardSource::AtPortOfAtom(atom_id, idx) => {
                        let link = &self.atoms[atom_id].args[*idx];
                        if let Some(opposite_type) = link.opposite_type {
                            if opposite_type != *type_ {
                                return Err(TransformError::GuardTypeMismatch {
                                    span: link.span,
                                    expected: *type_,
                                    found: opposite_type,
                                });
                            }
                        } else {
                            let atom = self.atoms.get_mut(atom_id).expect("atom must exist");
                            atom.args[*idx].opposite_type = Some(*type_);
                            for link in free_links.values_mut() {
                                if link.opposite == atom.args[*idx].this {
                                    link.this_type = Some(*type_);
                                }
                            }
                        }
                    }
                    GuardSource::Variable(def) => {
                        if let Some(atoms) = self.var_atoms.get(def) {
                            for atom_id in atoms {
                                let atom = self.atoms.get_mut(atom_id).expect("atom must exist");
                                if let Some(existing_type) = atom.type_ {
                                    if existing_type != *type_ {
                                        return Err(TransformError::GuardTypeMismatch {
                                            span: atom.span(),
                                            expected: *type_,
                                            found: existing_type,
                                        });
                                    }
                                } else {
                                    atom.type_ = Some(*type_);
                                }
                            }
                        }
                    }
                    GuardSource::Placeholder(name) => {
                        if let Some(link) = free_links.get_mut(name) {
                            if let Some((id, idx)) = link.opposite.id_index() {
                                let atom = self.atoms.get_mut(&id).expect("atom must exist");
                                atom.args[idx].opposite_type = Some(*type_);
                                link.this_type = Some(*type_);
                                substitute = Some(GuardSource::AtPortOfAtom(id, idx));
                            }
                        }
                        if substitute.is_none() {
                            for hl in self.hyperlinks.values() {
                                if hl.name == *name {
                                    let op = hl.args[0].opposite;
                                    let (id, idx) =
                                        op.id_index().expect("hyperlink should point to atom");
                                    substitute = Some(GuardSource::AtPortOfAtom(id, idx));
                                    break;
                                }
                            }
                        }
                        if substitute.is_none() {
                            return Err(TransformError::UnconstrainedLink {
                                link: name.clone(),
                                span: self.span,
                            });
                        }
                    }
                }

                if let Some(substitute) = substitute {
                    *var = substitute;
                }

                Ok(*type_)
            }
            GuardNode::Int(_) => Ok(ProcessConstraint::Int),
            GuardNode::Float(_) => Ok(ProcessConstraint::Float),
            GuardNode::Constraint(_, _) => Ok(*type_),
            GuardNode::Binary(op, lhs, rhs) => {
                let expected = (*op).into();
                let lhs_type =
                    self.substitute_link_in_guard_with_type(lhs, &expected, free_links)?;
                let rhs_type =
                    self.substitute_link_in_guard_with_type(rhs, &expected, free_links)?;
                if lhs_type != rhs_type {
                    return Err(TransformError::GuardTypeMismatch {
                        span: self.span,
                        expected: lhs_type,
                        found: rhs_type,
                    });
                }
                Ok(lhs_type)
            }
            GuardNode::Function(sig, args) => {
                for (arg, arg_type) in args.iter_mut().zip(sig.args.iter().cycle()) {
                    self.substitute_link_in_guard_with_type(arg, arg_type, free_links)?;
                }
                Ok(sig.ret)
            }
        }
    }

    fn apply_unification_pass(&mut self, mem_id: MembraneId) {
        let child_membranes: Vec<_> = self.membranes[&mem_id].membranes.iter().copied().collect();
        for mem in child_membranes {
            self.apply_unification_pass(mem);
        }

        let atom_ids: Vec<_> = self.membranes[&mem_id].atoms.iter().copied().collect();
        let mut connectors = vec![];
        let mut remove = vec![];

        for atom_id in atom_ids {
            let atom = self.atoms.get(&atom_id).expect("atom must exist");
            if atom.name == "=" && atom.args.len() == 2 {
                connectors.push(atom_id);
            }
        }

        for connector in &connectors {
            let atom = self.atoms.get_mut(connector).expect("atom must exist");
            let left = atom.args[0].opposite;
            let left_type = atom.args[0].opposite_type;
            let right = atom.args[1].opposite;
            let right_type = atom.args[1].opposite_type;
            let left_name = atom.args[0].name.clone();
            let right_name = atom.args[1].name.clone();

            if atom.parent == self.body && left.is_head() && right.is_head() {
                continue;
            }

            match (left.id_index(), right.id_index()) {
                (Some((id, idx)), None) => {
                    if !self.vars.contains_key(&id) {
                        let atom = self.atoms.get_mut(&id).expect("atom must exist");
                        atom.args[idx].opposite = right;
                        atom.args[idx].opposite_type = right_type;
                        atom.args[idx].name = right_name;
                    }
                }
                (None, Some((id, idx))) => {
                    if !self.vars.contains_key(&id) {
                        let atom = self.atoms.get_mut(&id).expect("atom must exist");
                        atom.args[idx].opposite = left;
                        atom.args[idx].opposite_type = left_type;
                        atom.args[idx].name = left_name;
                    }
                }
                (Some((left_id, left_idx)), Some((right_id, right_idx))) => {
                    if !self.vars.contains_key(&left_id) {
                        let left_atom = self.atoms.get_mut(&left_id).expect("atom must exist");
                        left_atom.args[left_idx].opposite = right;
                        left_atom.args[left_idx].opposite_type = right_type;
                        left_atom.args[left_idx].name = right_name;
                    }
                    if !self.vars.contains_key(&right_id) {
                        let right_atom = self.atoms.get_mut(&right_id).expect("atom must exist");
                        right_atom.args[right_idx].opposite = left;
                        right_atom.args[right_idx].opposite_type = left_type;
                    }
                }
                (None, None) => {}
            }

            remove.push(*connector);
        }

        let membrane = self
            .membranes
            .get_mut(&mem_id)
            .expect("membrane must exist");
        for connector in remove {
            self.atoms.remove(&connector);
            membrane.atoms.remove(&connector);
        }
    }

    fn temp_atom_name_for_guard(
        guard: &Guard,
        rule_span: Span,
        link_name: &str,
        variable_id: VariableId,
    ) -> Result<String, TransformError> {
        let variable = guard.get(variable_id).expect("guard variable must exist");
        Ok(match variable.ty {
            Some(ProcessConstraint::Int) => "_int".to_string(),
            Some(ProcessConstraint::Float) => "_float".to_string(),
            Some(ProcessConstraint::String) => "_string".to_string(),
            Some(constraint) => {
                return Err(TransformError::UnsupportedGuardConstraint {
                    span: rule_span,
                    constraint,
                })
            }
            None => link_name.to_string(),
        })
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for id in &self.membranes[&self.head].atoms {
            let atom = &self.atoms[id];
            write!(f, "[{}]{}(", id, atom.name)?;
            let args = atom
                .args
                .iter()
                .map(|link| link.name.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{}),", args)?;
        }

        write!(f, " :- ")?;

        for id in &self.membranes[&self.body].atoms {
            let atom = &self.atoms[id];
            write!(f, "[{}]{}(", id, atom.name)?;
            let args = atom
                .args
                .iter()
                .map(|link| link.name.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{}),", args)?;
        }

        write!(f, ".")
    }
}
