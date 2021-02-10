use super::Gc;
use super::Object;
use crate::compiler::chunk::Chunk;

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
