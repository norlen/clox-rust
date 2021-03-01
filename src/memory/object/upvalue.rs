use std::fmt;

use super::Object;
use crate::{memory::GC, vm::value::Value, vm::vm::VM};

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
    /// Creates a new upvalue, these are by default `Open`.
    pub fn new(local_index: usize) -> Self {
        Self::Open(local_index)
    }

    /// Close the upvalue, i.e. instead of having an index into the stack
    /// the actual owned value must be supplied and is stored withing the
    /// upvalue.
    pub fn close(&mut self, value: Value) {
        *self = Upvalue::Closed(value);
    }

    /// Returns the index if this is an open upvalue, otherwise it will panic.
    pub fn as_open(&self) -> usize {
        match self {
            Upvalue::Open(index) => *index,
            Upvalue::Closed(_) => panic!("Expected open upvalue"),
        }
    }

    /// Set the value for an open upvalue, will panic if the upvalue is closed.
    pub fn set(&mut self, local_index: usize) {
        match self {
            Upvalue::Open(index) => *index = local_index,
            _ => panic!("Expected open upvalue"),
        }
    }

    /// Returns the `Value` this upvalue holds, if it is open it will grab it from the stack,
    /// and if it is closed it will simply be returned.
    pub fn get(&self, vm: &VM) -> Value {
        match self {
            Upvalue::Open(index) => vm.stack.get(*index).unwrap().clone(),
            Upvalue::Closed(value) => value.clone(),
        }
    }
}

impl Object for Upvalue {
    fn trace_references(&self, gc: &mut GC) {
        match self {
            Upvalue::Open(_) => {} // Do nothing.
            Upvalue::Closed(closed) => gc.mark_value(*closed),
        }
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Upvalue>()
    }
}

impl fmt::Display for Upvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Upvalue::Open(index) => {
                write!(f, "open upvalue: idx {}", index)
            }
            Upvalue::Closed(val) => {
                write!(f, "closed upvalue: {}", val)
            }
        }
    }
}
