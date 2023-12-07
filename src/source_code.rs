use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// A source code file, with line and column information
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SourceCode {
    /// The name of the file
    name: String,
    /// The path to the file
    path: PathBuf,
    /// The source code
    source: String,
    /// offset of each line in the source
    lines: Vec<usize>,
}

impl SourceCode {
    /// Create a new SourceCode from a file
    ///
    /// The file should exist and be readable by the current user.
    ///
    /// Also, the file should be encoded in UTF-8
    pub fn new(file: &Path) -> SourceCode {
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
        SourceCode {
            name,
            path,
            source,
            lines,
        }
    }

    /// Create a new SourceCode from a string
    pub fn phony(source: String) -> SourceCode {
        let lines = source
            .lines()
            .scan(0, |state, line| {
                let start = *state;
                *state += line.len() + 1;
                Some(start)
            })
            .collect();
        SourceCode {
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

impl From<&Path> for SourceCode {
    fn from(other: &Path) -> SourceCode {
        SourceCode::new(other)
    }
}
