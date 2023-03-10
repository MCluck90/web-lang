use std::collections::HashMap;

use crate::phases::shared::{ObjectType, Type};

pub fn build_built_in_types() -> HashMap<Type, ObjectType> {
    let mut hash_map: HashMap<Type, ObjectType> = HashMap::new();
    hash_map.insert(Type::Bool, build_bool_type());
    hash_map.insert(Type::Int, build_int_type());
    hash_map.insert(Type::String, build_string_type());
    hash_map.insert(
        Type::List(Box::new(Type::Unknown)).to_base_type(),
        build_shared_list_type(),
    );
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

fn build_shared_list_type() -> ObjectType {
    ObjectType {
        key_to_type: [("length", Type::Int)]
            .iter()
            .map(|(key, type_)| (key.to_string(), Box::new(type_.clone())))
            .collect(),
    }
}

pub fn build_list_type(element_type: &Type) -> ObjectType {
    let mut shared = build_shared_list_type();
    shared.key_to_type.insert(
        "push".to_owned(),
        Box::new(Type::Function {
            parameters: vec![element_type.clone()],
            return_type: Box::new(Type::Void),
        }),
    );
    shared.key_to_type.insert(
        "pop".to_owned(),
        Box::new(Type::Function {
            parameters: Vec::new(),
            return_type: Box::new(element_type.clone()),
        }),
    );
    shared.key_to_type.insert(
        "last".to_owned(),
        Box::new(Type::Function {
            parameters: Vec::new(),
            return_type: Box::new(element_type.clone()),
        }),
    );
    shared.key_to_type.insert(
        "shallow-copy".to_owned(),
        Box::new(Type::Function {
            parameters: Vec::new(),
            return_type: Box::new(Type::List(Box::new(element_type.clone()))),
        }),
    );
    ObjectType {
        key_to_type: shared.key_to_type,
    }
}

pub fn build_future_type(value_type: &Type) -> ObjectType {
    ObjectType {
        key_to_type: [(
            "then",
            Type::Function {
                parameters: vec![Type::Function {
                    parameters: vec![value_type.clone()],
                    return_type: Box::new(Type::Void),
                }],
                return_type: Box::new(Type::Generic {
                    base: Box::new(Type::Custom("Future".into())),
                    parameters: vec![value_type.clone()],
                }),
            },
        )]
        .iter()
        .map(|(key, type_)| (key.to_string(), Box::new(type_.clone())))
        .collect(),
    }
}
