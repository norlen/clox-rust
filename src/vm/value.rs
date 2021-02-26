use crate::memory::{BoundMethod, Class, Closure, Function, Gc, Instance, NativeFn, Upvalue};
use std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(Gc<String>),
    Native(Gc<NativeFn>),
    Class(Gc<Class>),
    Instance(Gc<Instance>),
    BoundMethod(Gc<BoundMethod>),
    Function(Gc<Function>),
    Closure(Gc<Closure>),
    Upvalue(Gc<Upvalue>),
}

impl From<Gc<String>> for Value {
    fn from(string: Gc<String>) -> Self {
        Value::String(string)
    }
}

impl From<Gc<NativeFn>> for Value {
    fn from(object: Gc<NativeFn>) -> Self {
        Value::Native(object)
    }
}

impl From<Gc<Class>> for Value {
    fn from(object: Gc<Class>) -> Self {
        Value::Class(object)
    }
}

impl From<Gc<Instance>> for Value {
    fn from(object: Gc<Instance>) -> Self {
        Value::Instance(object)
    }
}

impl From<Gc<BoundMethod>> for Value {
    fn from(object: Gc<BoundMethod>) -> Self {
        Value::BoundMethod(object)
    }
}

impl From<Gc<Function>> for Value {
    fn from(object: Gc<Function>) -> Self {
        Value::Function(object)
    }
}

impl From<Gc<Closure>> for Value {
    fn from(object: Gc<Closure>) -> Self {
        Value::Closure(object)
    }
}

impl From<Gc<Upvalue>> for Value {
    fn from(object: Gc<Upvalue>) -> Self {
        Value::Upvalue(object)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v.as_ref()),
            Value::Native(v) => write!(f, "{:?}", v.as_ref()),
            Value::Class(v) => write!(f, "{}", v.as_ref()),
            Value::Instance(v) => write!(f, "{}", v.as_ref()),
            Value::BoundMethod(v) => write!(f, "{}", v.as_ref()),
            Value::Function(v) => write!(f, "{}", v.as_ref()),
            Value::Closure(v) => write!(f, "{}", v.as_ref()),
            Value::Upvalue(v) => write!(f, "{}", v.as_ref()),
        }
    }
}

impl Value {
    pub fn equals(&self, rhs: &Value) -> Option<bool> {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Some(lhs == rhs),
            (Value::Nil, Value::Nil) => Some(true),
            (Value::Bool(lhs), Value::Bool(rhs)) => Some(lhs == rhs),
            (Value::String(lhs), Value::String(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::Native(lhs), Value::Native(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::Class(lhs), Value::Class(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::Instance(lhs), Value::Instance(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::BoundMethod(lhs), Value::BoundMethod(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::Function(lhs), Value::Function(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::Closure(lhs), Value::Closure(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            (Value::Upvalue(lhs), Value::Upvalue(rhs)) => {
                Some(std::ptr::eq(lhs.ptr.as_ptr(), rhs.ptr.as_ptr()))
            }
            _ => None,
        }
    }

    /// Returns the value as a `Gc<String>`, panics if the value contains any other variant.
    pub fn as_string_object(&self) -> Gc<String> {
        match self {
            Value::String(o) => *o,
            _ => panic!("Expected string string"),
        }
    }

    /// Returns the value as a `Gc<Class>`, panics if the value contains any other variant.
    pub fn as_class(&self) -> Gc<Class> {
        match self {
            Value::Class(o) => *o,
            _ => panic!("Expected class object"),
        }
    }

    /// Returns the value as a `Gc<Instance>`, panics if the value contains any other variant.
    pub fn as_instance(&self) -> Gc<Instance> {
        match self {
            Value::Instance(o) => *o,
            _ => panic!("Expected instance object"),
        }
    }

    /// Returns the value as a `Gc<Function>`, panics if the value contains any other variant.
    pub fn as_function(&self) -> Gc<Function> {
        match self {
            Value::Function(o) => *o,
            _ => panic!("Expected function object"),
        }
    }

    /// Returns the value as a `Gc<Closure>`, panics if the value contains any other variant.
    pub fn as_closure(&self) -> Gc<Closure> {
        match self {
            Value::Closure(o) => *o,
            _ => panic!("Expected closure object"),
        }
    }
}
