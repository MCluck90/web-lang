use core::fmt;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ObjectType {
    pub key_to_type: HashMap<String, Box<Type>>,
}

impl std::hash::Hash for ObjectType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (key, value) in &self.key_to_type {
            state.write(key.as_bytes());
            value.hash(state);
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Unknown,
    Void,
    Bool,
    Int,
    String,
    Object(ObjectType),
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Custom(String),
    Union(Box<Type>, Box<Type>),
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
                "({}): {}",
                parameters
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(", "),
                format!("{}", return_type)
            ),
            Type::Custom(custom) => write!(f, "{}", custom),
            Type::Object(ObjectType { key_to_type }) => write!(
                f,
                "{{ {} }}",
                key_to_type
                    .iter()
                    .map(|(key, type_)| format!("{}: {}", key, type_))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Type::Union(left, right) => write!(f, "{} | {}", left, right),
        }
    }
}
