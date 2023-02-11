use crate::phases::shared::Type;

use super::symbol_table::{SymbolTable, TypeId, TypeSymbol};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EnvironmentType {
    Frontend,
    Backend,
    Isomorphic,
}

pub fn add_environment_to_table(mut symbol_table: SymbolTable) -> SymbolTable {
    macro_rules! add_primitive_type {
        ($name:ident,$type:ident) => {
            symbol_table.set_type(
                TypeId(stringify!($name).to_string()),
                TypeSymbol {
                    type_: Type::$type,
                    name: Some(stringify!($name).to_string()),
                },
            );
        };
    }

    add_primitive_type!(bool, Bool);
    add_primitive_type!(int, Int);
    add_primitive_type!(string, String);
    add_primitive_type!(void, Void);
    add_primitive_type!(unknown, Unknown);

    symbol_table
}
