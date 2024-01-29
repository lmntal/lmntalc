use std::{
    cmp,
    fmt::Display,
    ops::{Add, Deref, Range, Sub},
    path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

/// A position in a CodeMap.
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug, Serialize, Deserialize)]
pub struct Pos(usize);

impl Pos {
    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

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

    /// Create a dummy span that points to the beginning of the file.
    pub const fn dummy() -> Self {
        Self {
            low: Pos(0),
            high: Pos(0),
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(val: Span) -> Self {
        val.low.as_usize()..val.high.as_usize()
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

/// A source code file, with line and column information
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Source {
    /// The name of the file
    name: String,
    /// The path to the file
    path: PathBuf,
    /// The source code
    source: String,
    /// offset of each line in the source
    lines: Vec<usize>,
}

impl Source {
    /// Create a new SourceCode from a file
    ///
    /// The file should exist and be readable by the current user.
    ///
    /// Also, the file should be encoded in UTF-8
    pub fn new(file: &Path) -> Source {
        let name = file.file_name().unwrap().to_str().unwrap().to_string();
        let path = file.to_path_buf();
        let source = std::fs::read_to_string(file).unwrap();
        let lines = source
            .lines()
            .scan(0, |state, line| {
                let start = *state;
                *state += line.len() + 1;
                Some(start)
            })
            .collect();
        Source {
            name,
            path,
            source,
            lines,
        }
    }

    /// Create a new SourceCode from a string
    pub fn phony(source: String) -> Source {
        let lines = source
            .lines()
            .scan(0, |state, line| {
                let start = *state;
                *state += line.len() + 1;
                Some(start)
            })
            .collect();
        Source {
            name: "<phony>".to_string(),
            path: PathBuf::new(),
            source,
            lines,
        }
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Get the line number at a given offset
    pub fn line_at_offset(&self, offset: usize) -> usize {
        match self.lines.binary_search(&offset) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    }

    /// Get the line and column number at a given offset
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = self.line_at_offset(offset);
        let col = offset - self.lines[line - 1] + 1;
        (line, col)
    }
}

impl From<&Path> for Source {
    fn from(other: &Path) -> Source {
        Source::new(other)
    }
}
