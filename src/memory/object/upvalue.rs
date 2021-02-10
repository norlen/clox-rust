use super::GC;
use crate::vm::value::Value;

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
