use crate::chunk::Chunk;
use std::fmt;
use crate::object::{Object, Allocated};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Allocated<Object>>,
    pub arity: i64,
    pub chunk: Chunk,
}

impl Function {
    pub fn function_name(&self) -> &str {
        let default = "<script>";
        if let Some(object) = &self.name {
            match &object.get().data {
                Object::String(object) => object.as_str(),
                _ => default
            }
        } else {
            default
        }
    }
}

pub type NativeFunction = fn(usize, &[Value]) -> Value;
#[derive(Clone)]
pub struct NativeFn {
    pub name: Allocated<Object>,
    pub fun: NativeFunction,
}

impl NativeFn {
    pub fn new(name: Allocated<Object>, fun: NativeFunction) -> Self {
        Self { name, fun }
    }

    pub fn function_name(&self) -> &str {
        let default = "<script>";
        match &self.name.get().data {
            Object::String(object) => object.as_str(),
            _ => default
        }
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "NativeFn")
    }
}

impl Function {
    pub fn blank() -> Self {
        Self {
            name: None,
            arity: 0,
            chunk: Chunk::new(),
        }
    }

    fn new(name: Allocated<Object>) -> Self {
        Self {
            name: Some(name),
            arity: 0,
            chunk: Chunk::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(Allocated<Object>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Number(v) => write!(f, "{}", v),
            Value::Object(v) => write!(f, "{:?}", v.get()),
        }
    }
}

impl Value {
    pub fn equals(lhs: Value, rhs: Value) -> Option<bool> {
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs == rhs),
            (Value::Nil, Value::Nil) => Some(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Some(lhs == rhs),
            (Value::Object(lhs), Value::Object(rhs)) => Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr())),
            _ => None,
        }
    }
}
