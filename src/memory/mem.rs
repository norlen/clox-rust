#![allow(dead_code)]

use std::fmt;
use std::{cell::Cell, ptr::NonNull};

use crate::{compiler::chunk::Chunk, vm::value::Value};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Gc<String>>,
    pub arity: i64,
    pub chunk: Chunk,
    pub num_upvalues: usize,
}

fn run() {

}

pub struct GC {
    objects: Vec<Box<dyn TraceReferences>>,
    temp_roots: Vec<NonNull<dyn TraceReferences>>,

    gray_list: Vec<NonNull<dyn TraceReferences>>,
}

pub struct Root<T> {
    data: Gc<T>,
}

impl GC {
    pub fn track<T>(&mut self, object: T) -> Gc<T>
    where T: TraceReferences + 'static
    {
        let mut object = Box::new(object);
        let gcd = Gc::new(object.as_mut());
        self.objects.push(object);
        gcd
    }

    pub fn track_root<T>(&mut self, object: T) -> Root<T>
    where T: TraceReferences + 'static
    {
        let mut object = Box::new(object);
        let gcd = Gc::new(object.as_mut());
        self.objects.push(object);
        self.temp_roots.push(gcd.ptr);
        Root {
            data: gcd
        }
    }

    fn mark(&mut self, object: impl TraceReferences)
    {
        // println!("{:?}", object);
        todo!()
    }

    fn mark_roots(&mut self) {
        self.temp_roots.iter().for_each(|root| self.mark(*root));
    }

    fn mark_value(&mut self, _value: Value) { todo!() }

    
}

pub struct Compiler {
    functions: Vec<Function>,
}

pub struct VM {
    stack: Vec<Value>,
}

pub struct Gc<T: ?Sized> {
    ptr: NonNull<T>,
}

impl<T> Gc<T>{
    fn new(ptr: &mut T) -> Self {
        Self { ptr: NonNull::new(ptr).unwrap() }
    }
}

impl<T: fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Gc {{ ptr: {:?} }}", self.ptr.as_ptr())
    }
}

impl<T> Copy for Gc<T> {}

impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub trait Trace {
    fn set_mark(&mut self, value: bool);
    fn marked(&self) -> bool;
    fn data_size(&self) -> usize;
}

pub struct Traced<T: ?Sized> {
    marked: Cell<bool>,
    data: T,
}

impl<T> Traced<T> {
    fn new(data: T) -> Self {
        Self {
            marked: Cell::new(false),
            data,
        }
    }
}

impl<T> Trace for Traced<T> {
    fn set_mark(&mut self, value: bool) {
        self.marked.set(value);
    }

    fn marked(&self) -> bool {
        self.marked.get()
    }

    fn data_size(&self) -> usize {
        std::mem::size_of::<T>()
    }
}

pub trait TraceReferences {
    fn blacken(&self, gc: &mut GC);
}

impl TraceReferences for String {
    fn blacken(&self, _gc: &mut GC) {}
}

impl TraceReferences for Function {
    fn blacken(&self, gc: &mut GC) {
        // For referenced function we want to first mark the function name, and then
        // everything in the constant list that's used by the code.
        if let Some(name) = self.name {
            gc.mark(name);
        }

        self.chunk.constants.iter().for_each(|constant| {
            gc.mark_value(constant.clone());
        });
    }
}