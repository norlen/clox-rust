use std::fmt;
use crate::object::{Object, Allocated, Closure, Function, NativeFn, Upvalue};

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

    pub fn equals2(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs == rhs),
            (Value::Nil, Value::Nil) => Some(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Some(lhs == rhs),
            (Value::Object(lhs), Value::Object(rhs)) => Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr())),
            _ => None,
        }
    }

    // TODO: Change the error type and return errors.
    pub fn as_object(&self) -> Result<Allocated<Object>, String> {
        self.as_object_ref().map(|o| o.clone())
    }

    pub fn as_object_ref(&self) -> Result<&Allocated<Object>, String> {
        match self {
            Value::Object(object) => Ok(object),
            _ => panic!("Expected object"),
        }
    }

    pub fn as_string(&self) -> Result<&String, String> {
        self.as_object_ref()?.get().data.as_string()
    }

    pub fn as_function(&self) -> Result<&Function, String> {
        self.as_object_ref()?.get().data.as_function()
    }

    pub fn as_native(&self) -> Result<&NativeFn, String> {
        self.as_object_ref()?.get().data.as_native()
    }

    pub fn as_closure(&self) -> Result<&Closure, String> {
        self.as_object_ref()?.get().data.as_closure()
    }

    pub fn as_upvalue(&self) -> Result<&Upvalue, String> {
        self.as_object_ref()?.get().data.as_upvalue()
    }
}

