use std::fmt;
use std::ptr;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(usize),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(ref v) => write!(f, "{}", v),
        }
    }
}

impl Value {
    pub fn equals(lhs: Value, rhs: Value) -> Option<bool> {
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs == rhs),
            (Value::Nil, Value::Nil) => Some(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Some(lhs == rhs),
            (Value::String(ref lhs), Value::String(ref rhs)) => Some(ptr::eq(lhs, rhs)),
            _ => None,
        }
    }
}
