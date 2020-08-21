use crate::memory::{Closure, Function, Gc, NativeFn, Object, Upvalue};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Object(Gc<Object>),
}

impl From<Gc<Object>> for Value {
    fn from(object: Gc<Object>) -> Self {
        Value::Object(object)
    }
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
    pub fn equals(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs == rhs),
            (Value::Nil, Value::Nil) => Some(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Some(lhs == rhs),
            (Value::Object(lhs), Value::Object(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            _ => None,
        }
    }

    pub fn as_object(&self) -> Gc<Object> {
        self.as_object_ref().clone()
    }

    pub fn as_object_ref(&self) -> &Gc<Object> {
        match self {
            Value::Object(object) => object,
            _ => panic!("Expected object"),
        }
    }

    pub fn as_string(&self) -> &String {
        self.as_object_ref().get().as_string()
    }

    pub fn as_function(&self) -> &Function {
        self.as_object_ref().get().as_function()
    }

    pub fn as_native(&self) -> &NativeFn {
        self.as_object_ref().get().as_native()
    }

    pub fn as_closure(&self) -> &Closure {
        self.as_object_ref().get().as_closure()
    }

    pub fn as_upvalue(&self) -> &Upvalue {
        self.as_object_ref().get().as_upvalue()
    }
}
