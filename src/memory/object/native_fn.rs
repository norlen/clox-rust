use std::fmt;

use super::Gc;
use crate::vm::value::Value;

pub type NativeFunction = fn(usize, &[Value]) -> Value;

#[derive(Clone)]
pub struct NativeFn {
    /// Name of the native function.
    pub name: Gc<String>,
    pub fun: NativeFunction,
}

impl NativeFn {
    pub fn new(name: Gc<String>, fun: NativeFunction) -> Self {
        Self { name, fun }
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
