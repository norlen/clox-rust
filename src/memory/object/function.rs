use super::Gc;
use crate::compiler::chunk::Chunk;
use std::fmt;

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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.function_name())
    }
}
