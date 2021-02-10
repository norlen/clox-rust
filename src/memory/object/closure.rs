use super::Gc;
use super::Object;

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Gc<Object>,
    pub upvalues: Vec<Gc<Object>>,
    pub upvalue_count: usize,
}

impl Closure {
    pub fn new(function: Gc<Object>) -> Self {
        let num_upvalues = function.as_function().num_upvalues;
        Self {
            function: function.clone(),
            upvalues: Vec::with_capacity(num_upvalues),
            upvalue_count: num_upvalues,
        }
    }
}
