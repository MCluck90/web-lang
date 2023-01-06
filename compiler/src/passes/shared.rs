use core::fmt;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NodeId {
    value: u32,
}

impl NodeId {
    pub const fn from_u32(value: u32) -> NodeId {
        NodeId { value }
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

// We use this just for the initial construction. We need to go over the tree later and generate unique IDs.
// We don't generate them as we parse the tree because, for example, repeated use of the same identifier should
// give the same ID.
pub const DUMMY_NODE_ID: NodeId = NodeId::from_u32(u32::MAX);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Unknown,
}

#[derive(Clone, Debug)]
pub struct Symbol {
    pub owner_id: NodeId,
    pub type_: Type,
}
impl Symbol {
    pub fn new(owner_id: NodeId) -> Self {
        Symbol {
            owner_id,
            type_: Type::Unknown,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    next_id: u32,
    table: HashMap<NodeId, Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            next_id: 0,
            table: HashMap::new(),
        }
    }

    pub fn generate_id(&mut self) -> NodeId {
        let node_id = NodeId::from_u32(self.next_id);
        self.next_id += 1;
        node_id
    }

    pub fn insert(&mut self, id: NodeId, symbol: Symbol) -> Option<Symbol> {
        self.table.insert(id, symbol)
    }
}
