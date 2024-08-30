use std::collections::HashMap;

macro_rules! id_with_parent {
    (
        $(
            $name:ident
        ),*
    ) => {
        $(
            /// A unique identifier for a process, which is a 64-bit unsigned integer
            ///
            /// The high 4 bytes indicate the parent process,
            /// and the low 4 bytes indicate the id of the process.
            ///
            /// The id will start from 1, and 0 is reserved for the root process.
            #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(u64);

            impl From<$name> for u32 {
                fn from(id: $name) -> Self {
                    0xFFFFFFFF & id.0 as u32
                }
            }

            impl From<$name> for u64 {
                fn from(id: $name) -> Self {
                    id.0
                }
            }

            impl From<u64> for $name {
                fn from(id: u64) -> Self {
                    Self(id)
                }
            }

            impl $name {
                #[allow(dead_code)]
                fn new(parent: u32, id: u32) -> Self {
                    let parent = parent as u64;
                    let id = id as u64;
                    Self((parent << 32) | id)
                }

                pub fn parent(&self) -> MembraneId {
                    ((self.0 >> 32) as u32).into()
                }

                pub fn id(&self) -> u32 {
                    (self.0 & 0xFFFFFFFF) as u32
                }
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}:{}", self.parent(), self.id())
                }
            }
        )*
    };
}

macro_rules! id_without_parent {
    (
        $(
            $name:ident, $data:ty
        ),*
    ) => {
        $(
            impl From<$name> for $data {
                fn from(id: $name) -> Self {
                    id.0
                }
            }

            impl From<$data> for $name {
                fn from(id: $data) -> Self {
                    Self(id)
                }
            }

            impl $name {
                pub fn new(id: $data) -> Self {
                    Self(id)
                }

                pub fn id(&self) -> $data {
                    self.0
                }
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}", self.id())
                }
            }
        )*
    };
}

/// A unique identifier for a membrane, which is a 32-bit unsigned integer
///
/// Because other processes ID contains parent membrane ID,
/// locate them is easy by comparing the high 4 bytes.
/// So, if we still store the parent membrane ID in the membrane ID,
/// we can't locate the membrane by comparing the high 4 bytes.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MembraneId(u32);

/// Hyperlink is actually an atom, but it is not stored in the atom table.
pub type HyperlinkId = AtomId;

id_with_parent! {
    RuleId,
    AtomId
}

id_without_parent! {
    MembraneId, u32
}

#[derive(Debug, Default, Clone)]
pub(super) struct IdGenerator {
    mem_counter: u32,
    atom_counter: HashMap<MembraneId, u32>,
    link_counter: u64,
}

impl IdGenerator {
    pub(super) fn next_membrane_id(&mut self) -> MembraneId {
        self.mem_counter += 1;
        MembraneId::new(self.mem_counter - 1)
    }

    pub(super) fn next_atom_id(&mut self, parent: MembraneId) -> AtomId {
        let counter = self.atom_counter.entry(parent).or_insert(0);
        *counter += 1;
        AtomId::new(parent.into(), *counter)
    }

    pub(super) fn next_link_id(&mut self) -> u64 {
        self.link_counter += 1;
        self.link_counter - 1
    }

    pub(super) fn next_hyperlink_id(&mut self) -> HyperlinkId {
        let counter = self.atom_counter.entry(u32::MAX.into()).or_insert(0);
        *counter += 1;
        AtomId::new(u32::MAX, *counter)
    }
}

#[test]
fn test_cmp_id() {
    let id1 = AtomId::new(1, 1);
    let id2 = AtomId::new(1, 2);
    let id3 = AtomId::new(2, 1);
    let id4 = AtomId::new(2, 2);
    assert!(id1 < id2);
    assert!(id2 < id3);
    assert!(id3 < id4);
}

#[test]
fn test_eq_id() {
    let id1 = AtomId::new(1, 1);
    let id2 = AtomId::new(1, 1);
    assert_eq!(id1, id2);
}
