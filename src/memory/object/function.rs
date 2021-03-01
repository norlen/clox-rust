use std::fmt;

use super::Object;
use crate::{
    compiler::chunk::Chunk,
    memory::{Gc, GC},
};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Gc<String>>,
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

    pub fn new(name: Gc<String>) -> Self {
        Self {
            name: Some(name),
            arity: 0,
            chunk: Chunk::new(),
            num_upvalues: 0,
        }
    }

    pub fn function_name(&self) -> &str {
        if let Some(object) = &self.name {
            object.as_ref()
        } else {
            "<script"
        }
    }
}

impl Object for Function {
    fn trace_references(&self, gc: &mut GC) {
        // For referenced function we want to first mark the function name, and then
        // everything in the constant list that's used by the code.
        if let Some(name) = self.name {
            gc.mark(name);
        }

        self.chunk.constants.iter().for_each(|constant| {
            gc.mark_value(constant.clone());
        });
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Function>()
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.function_name())
    }
}
