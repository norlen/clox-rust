use crate::chunk;
use std::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(usize),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arity: i64,
    pub chunk: chunk::Chunk,
}

impl Function {
    pub fn blank() -> Self {
        Self {
            name: String::new(),
            arity: 0,
            chunk: chunk::Chunk::new(),
        }
    }

    fn new<T: Into<String>>(name: T) -> Self {
        Self {
            name: name.into(),
            arity: 0,
            chunk: chunk::Chunk::new(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(ref v) => write!(f, "{}", v),
            Value::Function(ref v) => write!(f, "fn {}", v.name),
        }
    }
}

impl Value {
    pub fn equals(lhs: Value, rhs: Value) -> Option<bool> {
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs == rhs),
            (Value::Nil, Value::Nil) => Some(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Some(lhs == rhs),
            (Value::String(ref lhs), Value::String(ref rhs)) => Some(lhs == rhs),
            (Value::Function(ref lhs), Value::Function(ref rhs)) => Some(lhs.name == rhs.name),
            _ => None,
        }
    }
}
