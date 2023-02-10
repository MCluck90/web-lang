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
    List(Box<Type>),
    Object(ObjectType),
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    },
    Custom(String),
    Union(Box<Type>, Box<Type>),
    Generic {
        base: Box<Type>,
        parameters: Vec<Type>,
    },
}
impl Type {
    /// Simplifies a type to it's base type.
    /// Useful for looking up properties on base types.
    pub fn to_base_type(&self) -> Self {
        match self {
            Type::Unknown => Type::Unknown,
            Type::Void => Type::Void,
            Type::Bool => Type::Bool,
            Type::Int => Type::Int,
            Type::String => Type::String,
            Type::List(_) => Self::get_base_list_type(),
            Type::Object(_) => Type::Object(ObjectType {
                key_to_type: HashMap::new(),
            }),
            Type::Function { .. } => Type::Function {
                parameters: Vec::new(),
                return_type: Box::new(Type::Unknown),
            },
            Type::Custom(_) => Type::Custom("".to_string()),
            Type::Union(_, _) => Type::Union(Box::new(Type::Unknown), Box::new(Type::Unknown)),
            Type::Generic {
                base,
                parameters: _,
            } => base.to_base_type(),
        }
    }

    pub fn get_base_list_type() -> Self {
        Type::List(Box::new(Type::Unknown))
    }

    pub fn is_primitive(&self) -> bool {
        match self {
            Type::Bool | Type::Int | Type::String => true,
            Type::Unknown
            | Type::Void
            | Type::List(_)
            | Type::Object(_)
            | Type::Function { .. }
            | Type::Custom(_)
            | Type::Union(_, _)
            | Type::Generic { .. } => false,
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unknown => write!(f, "unknown"),
            Type::Void => write!(f, "void"),
            Type::Bool => write!(f, "bool"),
            Type::Int => write!(f, "int"),
            Type::String => write!(f, "string"),
            Type::List(t) => write!(f, "[{}]", t),
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
            Type::Generic { base, parameters } => write!(
                f,
                "{}<{}>",
                base,
                parameters
                    .iter()
                    .map(|p| format!("{}", p))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
        }
    }
}
