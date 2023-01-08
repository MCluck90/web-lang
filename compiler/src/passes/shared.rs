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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Void,
    Bool,
    Int,
    String,
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Custom(String),
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Void => write!(f, "void"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::String => write!(f, "string"),
            Type::Function {
                parameters,
                return_type,
            } => write!(
                f,
                "({}) -> {}",
                parameters
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(", "),
                format!("{}", return_type)
            ),
            Type::Custom(custom) => write!(f, "{}", custom),
        }
    }
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

    pub fn get(&self, id: &NodeId) -> Option<&Symbol> {
        self.table.get(id)
    }

    pub fn get_mut(&mut self, id: &NodeId) -> Option<&mut Symbol> {
        self.table.get_mut(id)
    }

    pub fn set_type(&mut self, id: &NodeId, type_: Type) -> bool {
        if let Some(mut symbol) = self.table.get_mut(id) {
            symbol.type_ = type_;
            true
        } else {
            false
        }
    }
}
