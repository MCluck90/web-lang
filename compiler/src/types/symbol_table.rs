use core::fmt;
use std::collections::HashMap;

use crate::phases::shared::Type;

use super::environment;

pub struct SymbolTable {
    values: HashMap<ValueId, ValueSymbol>,
    types: HashMap<TypeId, TypeSymbol>,
}
impl SymbolTable {
    pub fn new() -> Self {
        environment::add_environment_to_table(SymbolTable {
            values: HashMap::new(),
            types: HashMap::new(),
        })
    }

    pub fn get_value(&self, id: &ValueId) -> Option<&ValueSymbol> {
        self.values.get(id)
    }

    pub fn set_value(&mut self, id: ValueId, symbol: ValueSymbol) {
        self.values.insert(id, symbol);
    }

    pub fn get_type(&self, id: &TypeId) -> Option<&TypeSymbol> {
        self.types.get(id)
    }

    pub fn set_type(&mut self, id: TypeId, symbol: TypeSymbol) {
        self.types.insert(id, symbol);
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct ValueId(pub String);
impl From<String> for ValueId {
    fn from(id: String) -> Self {
        ValueId(id)
    }
}
impl From<&str> for ValueId {
    fn from(id: &str) -> Self {
        ValueId(id.to_string())
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct TypeId(pub String);
impl From<String> for TypeId {
    fn from(id: String) -> Self {
        TypeId(id)
    }
}
impl From<&str> for TypeId {
    fn from(id: &str) -> Self {
        TypeId(id.to_string())
    }
}

pub struct ValueSymbol {
    pub type_id: Option<TypeId>,
    pub type_: Type,
    pub is_mutable: bool,
}
impl ValueSymbol {
    pub fn new() -> ValueSymbol {
        ValueSymbol {
            type_id: None,
            type_: Type::Unknown,
            is_mutable: false,
        }
    }

    pub fn with_type(self, type_: Type) -> Self {
        ValueSymbol { type_, ..self }
    }

    pub fn with_mutability(self, is_mutable: bool) -> Self {
        ValueSymbol { is_mutable, ..self }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct TypeSymbol {
    pub type_: Type,
    pub name: Option<String>,
}
impl TypeSymbol {
    pub fn bool() -> Self {
        TypeSymbol {
            type_: Type::Bool,
            name: Some("bool".into()),
        }
    }
    pub fn int() -> Self {
        TypeSymbol {
            type_: Type::Int,
            name: Some("int".into()),
        }
    }
    pub fn string() -> Self {
        TypeSymbol {
            type_: Type::String,
            name: Some("string".into()),
        }
    }
}

impl fmt::Display for TypeSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match &self.name {
                Some(name) => name.clone(),
                None => self.type_.to_string(),
            }
        )
    }
}

impl From<Type> for TypeSymbol {
    fn from(value: Type) -> Self {
        TypeSymbol {
            type_: value,
            name: None,
        }
    }
}
