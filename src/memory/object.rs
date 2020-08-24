use colored::*;
use std::fmt::{self, Debug};

use super::{Gc, GC};
use crate::compiler::chunk::Chunk;
use crate::debug::LOG_OBJECT;
use crate::vm::value::Value;

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Function(Function),
    Native(NativeFn),
    Closure(Closure),
    Upvalue(Upvalue),
}

impl Drop for Object {
    fn drop(&mut self) {
        if LOG_OBJECT {
            println!("{}\tDROP Value: {:?}", "[OBJECT]".purple(), self);
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(v) => write!(f, "{}", v.as_str()),
            Object::Function(v) => write!(f, "<fn {}>", v.function_name()),
            Object::Native(v) => write!(f, "<native fn {}>", v.function_name()),
            Object::Closure(v) => {
                write!(f, "<closure {}>", v.function.as_function().function_name())
            }
            Object::Upvalue(_v) => write!(f, "upvalue"),
        }
    }
}

impl Object {
    pub fn as_string(&self) -> &String {
        match self {
            Object::String(string) => string,
            _ => panic!("Expected string"),
        }
    }

    pub fn as_function(&self) -> &Function {
        match self {
            Object::Function(function) => function,
            _ => panic!("Expected function"),
        }
    }

    // pub fn as_native(&self) -> &NativeFn {
    //     match self {
    //         Object::Native(native) => native,
    //         _ => panic!("Expected native function"),
    //     }
    // }

    // pub fn as_closure(&self) -> &Closure {
    //     match self {
    //         Object::Closure(closure) => closure,
    //         _ => panic!("Expected closure"),
    //     }
    // }

    pub fn as_upvalue(&self) -> &Upvalue {
        match self {
            Object::Upvalue(upvalue) => upvalue,
            _ => panic!("Expected upvalue"),
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut Closure {
        match self {
            Object::Closure(closure) => closure,
            _ => panic!("Expected closure"),
        }
    }

    pub fn as_upvalue_mut(&mut self) -> &mut Upvalue {
        match self {
            Object::Upvalue(upvalue) => upvalue,
            _ => panic!("Expected upvalue"),
        }
    }
}

// #[derive(Debug, Clone)]
// pub struct Upvalue {
//     pub location: *mut Value,
//     pub closed: Option<Value>,
// }

/// Upvalue holds references to a stack variable used in a closure. This allows closures
/// to close over variables. When those variables are popped off the stack the upvalue
/// becomes closed and the upvalue owns the Value.
#[derive(Debug, Clone)]
pub enum Upvalue {
    /// An open upvalue points to the stack.
    Open(usize),

    /// An upvalue is closed when we have to lift the value off the stack.
    Closed(Value),
}

impl Upvalue {
    pub fn new(local_index: usize) -> Self {
        Self::Open(local_index)
    }

    pub fn close(&mut self, value: Value) {
        *self = Upvalue::Closed(value);
    }

    pub fn as_open(&self) -> usize {
        match self {
            Upvalue::Open(index) => *index,
            Upvalue::Closed(_) => panic!("Expected open upvalue"),
        }
    }

    pub fn set(&mut self, local_index: usize) {
        match self {
            Upvalue::Open(index) => *index = local_index,
            _ => panic!("Expected open upvalue"),
        }
    }

    pub fn get(&self, gc: &GC) -> Value {
        match self {
            Upvalue::Open(index) => gc.stack.get(*index).unwrap().clone(),
            Upvalue::Closed(value) => value.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Gc<Object>>,
    pub arity: i64,
    pub chunk: Chunk,
    pub num_upvalues: usize,
}

impl Function {
    pub fn blank() -> Self {
        Self {
            name: None,
            arity: 0,
            chunk: Chunk::new(),
            num_upvalues: 0,
        }
    }

    pub fn new(name: Gc<Object>) -> Self {
        Self {
            name: Some(name),
            arity: 0,
            chunk: Chunk::new(),
            num_upvalues: 0,
        }
    }

    pub fn function_name(&self) -> &str {
        let default = "<script>";
        if let Some(object) = &self.name {
            match &object.get() {
                Object::String(object) => object.as_str(),
                _ => default,
            }
        } else {
            default
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Gc<Object>,
    pub upvalues: Vec<Gc<Object>>,
    pub upvalue_count: usize,
}

impl Closure {
    pub fn new(function: Gc<Object>) -> Self {
        let num_upvalues = function.as_function().num_upvalues;
        Self {
            function: function.clone(),
            upvalues: Vec::with_capacity(num_upvalues),
            upvalue_count: num_upvalues,
        }
    }
}

pub type NativeFunction = fn(usize, &[Value]) -> Value;
#[derive(Clone)]
pub struct NativeFn {
    pub name: Gc<Object>,
    pub fun: NativeFunction,
}

impl NativeFn {
    pub fn new(name: Gc<Object>, fun: NativeFunction) -> Self {
        Self { name, fun }
    }

    pub fn function_name(&self) -> &str {
        let default = "<script>";
        match self.name.get() {
            Object::String(object) => object.as_str(),
            _ => default,
        }
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "NativeFn")
    }
}
