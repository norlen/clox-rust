use std::fmt;

use super::{Function, Object, Upvalue};
use crate::memory::{Gc, GC};

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Gc<Function>,
    pub upvalues: Vec<Gc<Upvalue>>,
    pub upvalue_count: usize,
}

impl Closure {
    pub fn new(function: Gc<Function>) -> Self {
        let num_upvalues = function.as_ref().num_upvalues;
        Self {
            function: function.clone(),
            upvalues: Vec::with_capacity(num_upvalues),
            upvalue_count: num_upvalues,
        }
    }
}

impl Object for Closure {
    fn trace_references(&self, gc: &mut GC) {
        gc.mark(self.function);
        self.upvalues.iter().for_each(|upvalue| gc.mark(*upvalue));
    }

    fn size(&self) -> usize {
        std::mem::size_of::<Closure>()
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure {}>", self.function.as_ref().function_name())
    }
}
