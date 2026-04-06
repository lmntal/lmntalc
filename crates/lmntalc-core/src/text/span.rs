use std::{
    cmp::{self, Ordering},
    fmt::Display,
    hash::{Hash, Hasher},
    ops::{Deref, Range, Sub},
};

use serde::{Deserialize, Serialize};

/// A position in a source file.
#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize)]
pub struct Pos {
    /// The character offset from the beginning of the file.
    pub offset: u32,
    /// The line number (0-based).
    pub line: u32,
    /// The column number (0-based).
    pub column: u32,
}

impl Hash for Pos {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.offset.hash(state);
    }
}

impl PartialEq for Pos {
    fn eq(&self, other: &Self) -> bool {
        self.offset == other.offset
    }
}

impl Eq for Pos {}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
    }
}

impl Pos {
    pub fn new(offset: u32, line: u32, column: u32) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }

    pub fn line_col(&self) -> (u32, u32) {
        (self.line, self.column)
    }

    pub fn as_u32(&self) -> u32 {
        self.offset
    }

    pub fn as_range(&self) -> Range<usize> {
        self.offset as usize..self.offset as usize
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.offset, self.line, self.column)
    }
}

impl Sub<Pos> for Pos {
    type Output = i32;

    fn sub(self, rhs: Self) -> Self::Output {
        (self.offset as i32) - (rhs.offset as i32)
    }
}

/// A range of text within a source file.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Span {
    low: Pos,
    high: Pos,
}

impl Span {
    pub fn new(low: Pos, high: Pos) -> Self {
        Self { low, high }
    }

    pub fn contains(&self, other: Span) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    pub fn low(&self) -> Pos {
        self.low
    }

    pub fn high(&self) -> Pos {
        self.high
    }

    pub fn len(&self) -> usize {
        (self.high - self.low) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn merge(&self, other: Span) -> Span {
        Span {
            low: cmp::min(self.low, other.low),
            high: cmp::max(self.high, other.high),
        }
    }

    pub const fn dummy() -> Self {
        Self {
            low: Pos {
                offset: 0,
                line: 0,
                column: 0,
            },
            high: Pos {
                offset: 0,
                line: 0,
                column: 0,
            },
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(val: Span) -> Self {
        (val.low.as_u32() as usize)..(val.high.as_u32() as usize)
    }
}

/// Associate a Span with a value of arbitrary type.
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn map_node<U, F: FnOnce(T) -> U>(self, op: F) -> Spanned<U> {
        Spanned {
            node: op(self.node),
            span: self.span,
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}
