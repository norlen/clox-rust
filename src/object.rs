use std::ptr::NonNull;
use std::fmt::{self, Debug};
use colored::*;

use crate::value::{Function, NativeFn};

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
        println!("{}\tAllocated::new() : {:?}", "[OBJECT]".purple(), ptr);
        Self {
            ptr: NonNull::new(ptr).unwrap(),
        }
    }
}

impl<T: Debug> std::ops::Deref for Allocated<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.get().data
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

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Function(Function),
    Native(NativeFn),
}

impl Drop for Object {
    fn drop(&mut self) {
        println!("{}\tDROP Value: {:?}", "[OBJECT]".purple(), self);
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(v) => write!(f, "{}", v),
            Object::Function(_v) => write!(f, "<fn>"),
            Object::Native(_v) => write!(f, "<native fn>"),
        }
    }
}
