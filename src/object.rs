use std::ptr::NonNull;
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Debug};
use colored::*;

use crate::value::Value;
use crate::chunk::Chunk;
use crate::debug::LOG_OBJECT;

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
            Object::String(v) => write!(f, "{}", v),
            Object::Function(_v) => write!(f, "<fn>"),
            Object::Native(_v) => write!(f, "<native fn>"),
            Object::Closure(_v) => write!(f, "<closure>"),
            Object::Upvalue(_v) => write!(f, "upvalue"),
        }
    }
}

impl Object {
    pub fn as_string(&self) -> Result<&String, String> {
        match self {
            Object::String(string) => Ok(string),
            _ => panic!("Expected string"),
        }
    }

    pub fn as_function(&self) -> Result<&Function, String> {
        match self {
            Object::Function(function) => Ok(function),
            _ => panic!("Expected function"),
        }
    }

    pub fn as_native(&self) -> Result<&NativeFn, String> {
        match self {
            Object::Native(native) => Ok(native),
            _ => panic!("Expected native function"),
        }
    }

    pub fn as_closure(&self) -> Result<&Closure, String> {
        match self {
            Object::Closure(closure) => Ok(closure),
            _ => panic!("Expected closure"),
        }
    }

    pub fn as_upvalue(&self) -> Result<&Upvalue, String> {
        match self {
            Object::Upvalue(upvalue) => Ok(upvalue),
            _ => panic!("Expected upvalue"),
        }
    }

    pub fn as_closure_mut(&mut self) -> Result<&mut Closure, String> {
        match self {
            Object::Closure(closure) => Ok(closure),
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

// pub enum Upvalue2 {
//     Open(NonNull<Value>),
//     Closed(Value),
// }

// impl Upvalue2 {
//     pub fn new(location: &mut Value) -> Self {
//         Self::Open(NonNull::new(location).unwrap())
//     }

//     pub fn set(&)
// }

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub location: *mut Value,
    pub closed: Option<Value>,
}

impl Upvalue {
    pub fn new(location: &mut Value) -> Self {
        Self {
            location: location as *mut _,
            closed: None,
        }
    }

    pub fn set(&mut self, value: Value) {
        unsafe {
            *self.location = value;
        }
    }

    pub fn get(&self) -> &Value {
        unsafe {
            self.location.as_ref().unwrap()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Allocated<Object>>,
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

    fn new(name: Allocated<Object>) -> Self {
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
            match &object.get().data {
                Object::String(object) => object.as_str(),
                _ => default
            }
        } else {
            default
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Allocated<Object>,
    pub upvalues: Vec<Allocated<Object>>,
    pub upvalue_count: usize,
}

impl Closure {
    pub fn new(function: Allocated<Object>) -> Self {
        let num_upvalues = function.as_function().unwrap().num_upvalues;
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

#[derive(Debug, Copy, Clone)]
pub struct Traced<T: Debug> {
    pub(super) marked: bool,
    pub data: T,
}

impl<T: Debug> Traced<T> {
    pub(super) fn new(data: T) -> Self {
        Self {
            marked: false,
            data,
        }
    }
}

// Holds a pointer to some allocated object. Used by the
// garbage collector.
#[derive(Debug, Copy, Clone)]
pub struct Allocated<T: Debug> {
    pub ptr: NonNull<Traced<T>>,
}

impl<T: Debug> Allocated<T> {
    pub(super) fn new(ptr: &mut Traced<T>) -> Self {
        if LOG_OBJECT {
            println!("{}\tAllocated::new() : {:?}", "[OBJECT]".purple(), ptr);
        }
        Self {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
}

impl<T: Debug> Deref for Allocated<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.get().data
    }
}

impl<T: Debug> DerefMut for Allocated<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.get_mut().data
    }
}

impl<T: Debug> Allocated<T> {
    pub fn get(&self) -> &Traced<T> {
        // Yep!
        unsafe { self.ptr.as_ref() }
    }

    pub fn get_mut(&mut self) -> &mut Traced<T> {
        // Yep again!
        unsafe { self.ptr.as_mut() }
    }
}
