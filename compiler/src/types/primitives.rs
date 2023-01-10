use std::collections::HashMap;

use crate::passes::shared::{ObjectType, Type};

pub fn build_primitive_types() -> HashMap<Type, ObjectType> {
    let mut hash_map: HashMap<Type, ObjectType> = HashMap::new();
    hash_map.insert(Type::Bool, build_bool_type());
    hash_map.insert(Type::Int, build_int_type());
    hash_map.insert(Type::String, build_string_type());
    hash_map
}

fn build_bool_type() -> ObjectType {
    ObjectType {
        key_to_type: [(
            "toString",
            Type::Function {
                parameters: Vec::new(),
                return_type: Box::new(Type::String),
            },
        )]
        .iter()
        .map(|(key, type_)| (key.to_string(), Box::new(type_.clone())))
        .collect(),
    }
}

fn build_int_type() -> ObjectType {
    ObjectType {
        key_to_type: [(
            "toString",
            Type::Function {
                parameters: Vec::new(),
                return_type: Box::new(Type::String),
            },
        )]
        .iter()
        .map(|(key, type_)| (key.to_string(), Box::new(type_.clone())))
        .collect(),
    }
}

fn build_string_type() -> ObjectType {
    ObjectType {
        key_to_type: [("length", Type::Int)]
            .iter()
            .map(|(key, type_)| (key.to_string(), Box::new(type_.clone())))
            .collect(),
    }
}
