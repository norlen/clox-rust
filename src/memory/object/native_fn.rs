use std::fmt;

use super::Object;
use crate::{
    memory::{Gc, GC},
    vm::value::Value,
};

/// Type all native functions are expected to have.
pub type NativeFunction = fn(usize, &[Value]) -> Value;

/// A native function is a rust function that can be called from within lox.
#[derive(Clone)]
pub struct NativeFn {
    /// Name of the native function.
    pub name: Gc<String>,

    /// The actual function to call.
    pub fun: NativeFunction,
}

impl NativeFn {
    /// Creates a new native function.
    pub fn new(name: Gc<String>, fun: NativeFunction) -> Self {
        Self { name, fun }
    }
}

impl Object for NativeFn {
    fn trace_references(&self, _gc: &mut GC) {
        // Nothing has to be done here.
    }

    fn size(&self) -> usize {
        std::mem::size_of::<NativeFn>()
    }
}

impl fmt::Debug for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "<native fn {}>", self.name.as_ref())
    }
}

impl fmt::Display for NativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn {}>", self.name.as_ref())
    }
}
