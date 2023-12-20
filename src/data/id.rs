use std::collections::HashMap;

macro_rules! ids {
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
                fn new(parent: u32, id: u32) -> Self {
                    let parent = parent as u64;
                    let id = id as u64;
                    Self((parent << 32) | id)
                }

                pub fn parent(&self) -> u32 {
                    (self.0 >> 32) as u32
                }

                pub fn id(&self) -> u32 {
                    (self.0 & 0xFFFFFFFF) as u32
                }
            }
        )*
    };
}

ids! {
    RuleId,
    AtomId,
    LinkId,
    HyperLinkId
}

/// A unique identifier for a membrane, which is a 32-bit unsigned integer
///
/// Because other processes ID contains parent membrane ID,
/// locate them is easy by comparing the high 4 bytes.
/// So, if we still store the parent membrane ID in the membrane ID,
/// we can't locate the membrane by comparing the high 4 bytes.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MembraneId(u32);

impl MembraneId {
    pub(super) fn new(id: u32) -> Self {
        Self(id)
    }

    pub(crate) fn void() -> Self {
        Self::new(u32::MAX)
    }

    pub(crate) fn head() -> Self {
        Self::new(u32::MAX - 1)
    }

    pub(crate) fn propagation() -> Self {
        Self::new(u32::MAX - 2)
    }

    pub(crate) fn body() -> Self {
        Self::new(u32::MAX - 3)
    }

    pub fn id(&self) -> u32 {
        self.0
    }
}

impl From<u32> for MembraneId {
    fn from(id: u32) -> Self {
        Self(id)
    }
}

impl From<MembraneId> for u32 {
    fn from(id: MembraneId) -> Self {
        id.0
    }
}

#[derive(Debug, Default, Clone)]
pub(super) struct IdGenerator {
    mem_counter: u32,
    atom_counter: HashMap<MembraneId, u32>,
    link_counter: HashMap<MembraneId, u32>,
    rule_counter: HashMap<MembraneId, u32>,
    hyperlink_counter: HashMap<MembraneId, u32>,
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

    pub(super) fn next_link_id(&mut self, parent: MembraneId) -> LinkId {
        let counter = self.link_counter.entry(parent).or_insert(0);
        *counter += 1;
        LinkId::new(parent.into(), *counter)
    }

    pub(super) fn next_rule_id(&mut self, parent: MembraneId) -> RuleId {
        let counter = self.rule_counter.entry(parent).or_insert(0);
        *counter += 1;
        RuleId::new(parent.into(), *counter)
    }

    pub(super) fn next_hyperlink_id(&mut self, parent: MembraneId) -> HyperLinkId {
        let counter = self.hyperlink_counter.entry(parent).or_insert(0);
        *counter += 1;
        HyperLinkId::new(parent.into(), *counter)
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
