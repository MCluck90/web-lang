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
    pub type_: TypeId,
}

pub struct TypeSymbol {
    pub base_type: Type,
}
