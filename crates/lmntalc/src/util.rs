use std::{
    cmp::{self, Ordering},
    fmt::Display,
    hash::{Hash, Hasher},
    ops::{Deref, Range, Sub},
    path::Path,
};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum OneOf<T1, T2> {
    Left(T1),
    Right(T2),
}

/// A position in a CodeMap.
#[derive(Copy, Clone, Debug, Default, Serialize, Deserialize)]
pub struct Pos {
    /// The character offset from the beginning of the file.
    pub offset: u32,
    /// The line number (0-based)
    pub line: u32,
    /// The column number (0-based)
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
        Some(self.offset.cmp(&other.offset))
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
    /// The uri of the file (e.g. `file:///path/to/file.lmn`)
    uri: String,
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
    pub fn from_file(file: &Path) -> Source {
        let name = file.file_name().unwrap().to_str().unwrap().to_string();
        let uri = format!("file://{}", file.to_str().unwrap());
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
            uri,
            source,
            lines,
        }
    }

    /// Create a new SourceCode from a string
    pub fn from_string(source: String) -> Source {
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
            uri: "phony://".to_string(),
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

    pub fn uri(&self) -> &str {
        &self.uri
    }

    /// Get the line number (0-based) at a given offset
    pub fn line_at_offset(&self, offset: usize) -> usize {
        match self.lines.binary_search(&offset) {
            Ok(line) => line,
            Err(line) => line - 1,
        }
    }

    /// Get the line and column number at a given offset
    ///
    /// The line and column numbers are 0-based
    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = self.line_at_offset(offset);
        let col = offset - self.lines[line];
        (line, col)
    }
}
