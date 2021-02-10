use std::fmt;

use super::Gc;
use super::Object;
use crate::vm::value::Value;

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
