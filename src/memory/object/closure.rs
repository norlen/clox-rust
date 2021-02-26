use super::{Function, Gc, Upvalue};
use std::fmt;

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

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure {}>", self.function.as_ref().function_name())
    }
}

impl Drop for Closure {
    fn drop(&mut self) {
        println!("Dropping closure: {:?}", self);
    }
}
