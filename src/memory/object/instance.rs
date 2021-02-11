use super::{Gc, Object};
use crate::vm::value::Value;
use std::collections::HashMap;

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
