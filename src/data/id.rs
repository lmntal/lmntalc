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
            }
        )*
    };
}

ids! {
    RuleId,
    MembraneId,
    AtomId,
    LinkId,
    HyperLinkId
}

impl MembraneId {
    pub(crate) fn root() -> Self {
        Self::new(0, 0)
    }
}

#[derive(Debug, Default)]
pub(super) struct IdGenerator {
    atom_counter: HashMap<u32, u32>,
    link_counter: HashMap<u32, u32>,
    membrane_counter: HashMap<u32, u32>,
    rule_counter: HashMap<u32, u32>,
    hyperlink_counter: HashMap<u32, u32>,
}

impl IdGenerator {
    pub(super) fn next_atom_id(&mut self, parent: MembraneId) -> AtomId {
        let counter = self.atom_counter.entry(parent.into()).or_insert(0);
        *counter += 1;
        AtomId::new(parent.into(), *counter)
    }

    pub(super) fn next_link_id(&mut self, parent: MembraneId) -> LinkId {
        let counter = self.link_counter.entry(parent.into()).or_insert(0);
        *counter += 1;
        LinkId::new(parent.into(), *counter)
    }

    pub(super) fn next_membrane_id(&mut self, parent: MembraneId) -> MembraneId {
        let counter = self.membrane_counter.entry(parent.into()).or_insert(0);
        *counter += 1;
        MembraneId::new(parent.into(), *counter)
    }

    pub(super) fn next_rule_id(&mut self, parent: MembraneId) -> RuleId {
        let counter = self.rule_counter.entry(parent.into()).or_insert(0);
        *counter += 1;
        RuleId::new(parent.into(), *counter)
    }

    pub(super) fn next_hyperlink_id(&mut self, parent: MembraneId) -> HyperLinkId {
        let counter = self.hyperlink_counter.entry(parent.into()).or_insert(0);
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
