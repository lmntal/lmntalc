use std::{
    cmp,
    ops::{Add, Deref, Sub},
};

use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Pos(usize);

impl<T> From<T> for Pos
where
    T: Into<usize>,
{
    fn from(other: T) -> Self {
        Self(other.into())
    }
}

impl Add<usize> for Pos {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Pos(self.0 + rhs)
    }
}

impl Sub<Pos> for Pos {
    type Output = isize;

    fn sub(self, rhs: Self) -> Self::Output {
        (self.0 as isize) - (rhs.0 as isize)
    }
}

/// A range of text within a CodeMap.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Span {
    /// The position in the codemap representing the first byte of the span.
    low: Pos,

    /// The position after the last byte of the span.
    high: Pos,
}

impl Span {
    pub fn new(low: Pos, high: Pos) -> Self {
        Self { low, high }
    }

    /// Makes a span from offsets relative to the start of this span.
    ///
    /// # Panics
    ///   * If `end < begin`
    ///   * If `end` is beyond the length of the span
    pub fn subspan(&self, begin: usize, end: usize) -> Span {
        assert!(end >= begin);
        assert!(self.low + end <= self.high);
        Span {
            low: self.low + begin,
            high: self.low + end,
        }
    }

    /// Checks if a span is contained within this span.
    pub fn contains(&self, other: Span) -> bool {
        self.low <= other.low && self.high >= other.high
    }

    /// The position in the codemap representing the first byte of the span.
    pub fn low(&self) -> Pos {
        self.low
    }

    /// The position after the last byte of the span.
    pub fn high(&self) -> Pos {
        self.high
    }

    /// The length in bytes of the text of the span
    pub fn len(&self) -> usize {
        (self.high - self.low) as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Create a span that encloses both `self` and `other`.
    pub fn merge(&self, other: Span) -> Span {
        Span {
            low: cmp::min(self.low, other.low),
            high: cmp::max(self.high, other.high),
        }
    }

    pub const fn dummy() -> Self {
        Self {
            low: Pos(0),
            high: Pos(0),
        }
    }
}

/// Associate a Span with a value of arbitrary type (e.g. an AST node).
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Maps a `Spanned<T>` to `Spanned<U>` by applying the function to the node,
    /// leaving the span untouched.
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
