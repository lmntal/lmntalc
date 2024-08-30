use lmntal_model::{guard::ProcessConstraint, Data};

/// Basic operations of LMNtal
#[derive(Debug, Clone)]
pub enum LMNtalIR {
    /// Create a new atom with specified name and arity
    CreateAtom {
        id: usize,
        name: String,
        arity: usize,
        data: Data,
    },
    /// Remove an atom with specified `id`
    ///
    /// Since it will be only used for in rule,
    /// the `id` will be the order of the pattern atom in the rule
    RemoveAtom {
        id: usize,
    },
    /// Remove an atom at specified `port` of atom with specified `id`
    RemoveAtomAt {
        id: usize,
        port: usize,
    },
    /// Clone an atom from specified `from` atom with specified `id`
    CloneAtom {
        id: usize,
        from_id: usize,
        from_port: usize,
    },

    /// Link atoms with specified ports
    Link {
        src: VarSource,
        dst: VarSource,
    },
    /// Link the atom `dst`'s port `dst_port` to the **atom linked to** `src`'s port `src_port`
    Relink {
        src: usize,
        src_port: usize,
        dst: VarSource,
    },

    /// Create a new hyperlink with specified name
    ///
    /// There is no explicit arity for hyperlink, since it is not fixed
    CreateHyperlink {
        id: usize,
        name: String,
    },
    /// Add an atom to a hyperlink
    LinkToHyperlink {
        atom: VarSource,
        hyperlink: VarSource,
    },
    /// Remove an atom from a hyperlink
    RemoveFromHyperlink {
        atom: VarSource,
        hyperlink: VarSource,
    },
    /// Fuse hyperlink `from` into hyperlink `into`, and remove `from`
    ///
    /// i.e. `into` will be the new hyperlink with union of atoms in `into` and `from`
    FuseHyperlink {
        into: VarSource,
        from: VarSource,
    },

    /// Find atoms with specified name and arity
    ///
    /// This will be translated into a range-based for loop since there may be multiple atoms meet the requirement
    FindAtom {
        id: usize,
        name: String,
        arity: usize,
    },
    /// Get the atom at specified `port` of atom with specified `id`
    GetAtomAtPort {
        id: usize,
        from: usize,
        port: usize,
        name: String,
        arity: usize,
    },
    /// Get the hyperlink at specified `port` of atom with specified `id`
    GetHyperlinkAtPort {
        id: usize,
        from: usize,
        port: usize,
    },
    /// Check if atoms (or hyperlinks) at id's port are equal
    AtomEqualityIdPort {
        /// List of atoms and their ports
        id_port_list: Vec<(usize, usize)>,
        /// Whether the atoms are equal or not
        eq: bool,
    },
    /// Check if atoms (or hyperlinks) are equal
    AtomEquality {
        /// List of atoms and their ports
        id_list: Vec<usize>,
        /// Whether the atoms are equal or not
        eq: bool,
        /// Whether they are hyperlinks or atoms
        hyperlinks: bool,
    },
    /// Check the type of an atom at atom `id`'s `port`
    CheckType {
        id: usize,
        port: usize,
        ty: ProcessConstraint,
    },
    /// Check the value using specified operation
    CheckValue(Operation),
    /// Define a temporary variable using specified operation with specified name and type
    DefineTempVar {
        id: usize,
        name: String,
        ty: ProcessConstraint,
        op: Operation,
    },
    Unify {
        into: VarSource,
        from: VarSource,
    },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum VarSource {
    Head(usize, usize),
    Variable(usize),
    Body(usize, usize),
}

impl VarSource {
    pub fn id(&self) -> usize {
        match self {
            VarSource::Head(id, _) => *id,
            VarSource::Variable(id) => *id,
            VarSource::Body(id, _) => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Literal(Literal),
    Variable {
        source: VarSource,
        ty_: ProcessConstraint,
    },
    BinaryOP {
        op: BinaryOperator,
        lhs: Box<Operation>,
        rhs: Box<Operation>,
    },
    UnaryOP {
        op: UnaryOperator,
        operand: Box<Operation>,
    },
    FunctionCall {
        name: String,
        args: Vec<Operation>,
        ty_: ProcessConstraint,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Not,
    Neg,
}
