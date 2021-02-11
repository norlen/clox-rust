use super::{Object, Gc};
use std::collections::HashMap;
use crate::vm::value::Value;

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Gc<Object>,
    pub fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Gc<Object>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}
