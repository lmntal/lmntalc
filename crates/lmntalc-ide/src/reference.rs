use std::collections::HashMap;

use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct IndexedSpan {
    span: Span,
}

impl IndexedSpan {
    fn new(span: Span) -> Self {
        Self { span }
    }

    fn contains_offset(self, offset: usize) -> bool {
        let low = self.span.low().offset as usize;
        let high = self.span.high().offset as usize;
        if self.span.is_empty() {
            low == offset
        } else {
            low <= offset && offset <= high
        }
    }
}

impl Ord for IndexedSpan {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.span
            .low()
            .cmp(&other.span.low())
            .then(self.span.high().cmp(&other.span.high()))
    }
}

impl PartialOrd for IndexedSpan {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Default)]
pub struct ReferenceIndex {
    symbols: Vec<IndexedSpan>,
    references: HashMap<usize, Vec<usize>>,
}

impl ReferenceIndex {
    pub fn new(reference_groups: Vec<Vec<Span>>, queryable_spans: Vec<Span>) -> Self {
        let mut symbols = Vec::new();
        let mut references = HashMap::new();

        for group in &reference_groups {
            for span in group {
                symbols.push(IndexedSpan::new(*span));
            }
        }

        for span in &queryable_spans {
            symbols.push(IndexedSpan::new(*span));
        }

        symbols.sort();
        symbols.dedup();

        let indices = symbols
            .iter()
            .enumerate()
            .map(|(index, span)| (*span, index))
            .collect::<HashMap<_, _>>();

        for group in &reference_groups {
            for (index, span) in group.iter().enumerate() {
                let current = *indices
                    .get(&IndexedSpan::new(*span))
                    .expect("group span must be indexed");
                let refs = references.entry(current).or_insert_with(Vec::new);
                for (other_index, other_span) in group.iter().enumerate() {
                    if index == other_index {
                        continue;
                    }
                    refs.push(
                        *indices
                            .get(&IndexedSpan::new(*other_span))
                            .expect("group span must be indexed"),
                    );
                }
            }
        }

        Self {
            symbols,
            references,
        }
    }

    pub fn references_at_offset(&self, offset: usize) -> Vec<Span> {
        self.group_at_offset(offset)
    }

    pub fn highlights_at_offset(&self, offset: usize) -> Vec<Span> {
        self.group_at_offset(offset)
    }

    fn group_at_offset(&self, offset: usize) -> Vec<Span> {
        let Some(index) = find(offset, &self.symbols) else {
            return Vec::new();
        };

        let mut spans = self
            .references
            .get(&index)
            .into_iter()
            .flat_map(|refs| refs.iter().copied())
            .map(|reference| self.symbols[reference].span)
            .collect::<Vec<_>>();
        spans.push(self.symbols[index].span);
        spans.sort_by_key(|span| span.low());
        spans.dedup();
        spans
    }
}

fn find(offset: usize, spans: &[IndexedSpan]) -> Option<usize> {
    if spans.is_empty() {
        return None;
    }

    let mut low = 0;
    let mut high = spans.len() - 1;
    while low <= high {
        let mid = (low + high) / 2;
        let mid_span = spans[mid];
        if mid_span.contains_offset(offset) {
            return Some(mid);
        }
        if mid_span.span.low().offset as usize <= offset {
            low = mid + 1;
        } else if mid == 0 {
            break;
        } else {
            high = mid - 1;
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Pos;

    fn span(low: u32, high: u32) -> Span {
        Span::new(Pos::new(low, 0, low), Pos::new(high, 0, high))
    }

    #[test]
    fn finds_group_members_with_self() {
        let references = ReferenceIndex::new(vec![vec![span(0, 1), span(3, 4)]], vec![span(0, 1)]);
        assert_eq!(references.references_at_offset(0).len(), 2);
        assert_eq!(references.highlights_at_offset(3).len(), 2);
        assert!(references.references_at_offset(10).is_empty());
    }
}
