use std::{io, path::Path};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Source {
    name: String,
    uri: String,
    source: String,
    lines: Vec<usize>,
}

impl Source {
    pub fn new(
        uri: impl Into<String>,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Source {
        let source = source.into();
        let lines = build_line_index(&source);
        Source {
            name: name.into(),
            uri: uri.into(),
            source,
            lines,
        }
    }

    pub fn from_file(file: &Path) -> io::Result<Source> {
        let name = file
            .file_name()
            .map(|name| name.to_string_lossy().into_owned())
            .unwrap_or_else(|| file.display().to_string());
        let uri = format!("file://{}", file.to_string_lossy());
        let source = std::fs::read_to_string(file)?;
        Ok(Source::new(uri, name, source))
    }

    pub fn from_string(source: String) -> Source {
        Source::new("phony://", "<phony>", source)
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

    pub fn line_at_offset(&self, offset: usize) -> usize {
        match self.lines.binary_search(&offset) {
            Ok(line) => line,
            Err(0) => 0,
            Err(line) => line - 1,
        }
    }

    pub fn line_col(&self, offset: usize) -> (usize, usize) {
        let line = self.line_at_offset(offset);
        let col = offset - self.lines[line];
        (line, col)
    }

    pub fn offset_at(&self, line: u32, column: u32) -> Option<usize> {
        let line = line as usize;
        let column = column as usize;
        let start = *self.lines.get(line)?;
        let source_len = self.source.chars().count();
        let is_last_line = line + 1 >= self.lines.len();
        let line_end = self.lines.get(line + 1).copied().unwrap_or(source_len);

        if is_last_line {
            (column <= line_end.saturating_sub(start)).then_some(start + column)
        } else {
            (column < line_end.saturating_sub(start)).then_some(start + column)
        }
    }
}

fn build_line_index(source: &str) -> Vec<usize> {
    let mut lines = vec![0];
    for (offset, ch) in source.chars().enumerate() {
        if ch == '\n' {
            lines.push(offset + 1);
        }
    }
    lines
}
