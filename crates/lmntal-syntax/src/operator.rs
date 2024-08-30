#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    IntPlus,
    IntMinus,
    FloatPlus,
    FloatMinus,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    LogicOp(LogicOp),
    ArithOp(ArithOp),
    CmpOp(CmpOp),
    HyperlinkOp(HyperlinkOp),
    Assign,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LogicOp {
    And,
    Or,
    Xor,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum ArithOp {
    /// Integer addition (`+`)
    IntAdd,
    /// Integer subtraction (`-`)
    IntSub,
    /// Integer multiplication (`*`)
    IntMul,
    /// Integer division (`/`)
    IntDiv,
    /// Integer power (`**`)
    IntPow,
    /// Integer modulo (`mod`)
    IntMod,

    /// Float addition (`+.`)
    FloatAdd,
    /// Float subtraction (`-.`)
    FloatSub,
    /// Float multiplication (`*.`)
    FloatMul,
    /// Float division (`/.`)
    FloatDiv,

    /// Arithmetic shift (`ash`)
    ArithmeticShift,
    /// Logical shift (`lsh`)
    LogicalShift,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    /// Integer greater than (`>`)
    IntGt,
    /// Integer less than (`<`)
    IntLt,
    /// Integer greater than or equal to (`>=`)
    IntGe,
    /// Integer less than or equal to (`<=`)
    IntLe,
    /// Integer equal to (`=:=`)
    IntEq,
    /// Integer not equal to (`=\=`)
    IntNe,

    /// Float greater than (`>.`)
    FloatGt,
    /// Float less than (`<.`)
    FloatLt,
    /// Float greater than or equal to (`>=.`)
    FloatGe,
    /// Float less than or equal to (`<=.`)
    FloatLe,
    /// Float equal to (`=:=.`)
    FloatEq,
    /// Float not equal to (`=\=.`)
    FloatNe,

    /// Ground equal to (`==`)
    GroundEq,
    /// Ground not equal to (`\=`)
    GroundNe,

    /// Unary equal to (`===`)
    UnaryEq,
    /// Unary not equal to (`\==`)
    UnaryNe,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum HyperlinkOp {
    /// Hyperlink fuse (`><` or '>*<' or '>+<')
    Fuse,
    /// Hyperlink unfuse (`<<` or `>>`)
    Unify,
}
