use super::{Gc, Object};
use crate::vm::value::Value;
use std::collections::HashMap;

/// A class declaration containg the name and all it's method.
#[derive(Debug, Clone)]
pub struct Class {
    /// Name of the class.
    pub name: Gc<Object>,

    /// Methods this class contains.
    pub methods: HashMap<String, Gc<Object>>,
}

impl Class {
    /// Creates a new class with `name`. Requires the name object to be a
    /// garbage collected string.
    pub fn new(name: Gc<Object>) -> Self {
        Class {
            name,
            methods: HashMap::new(),
        }
    }
}
/// An instance of a class.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The class this instance is created from.
    pub class: Gc<Object>,
    
    /// Values for all the fields set.
    pub fields: HashMap<String, Value>,
}

impl Instance {
    /// Creates a new class instance, `class` must be a valid garbage collected class.
    pub fn new(class: Gc<Object>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

/// A method bound to a specific instance.
#[derive(Debug, Clone)]
pub struct BoundMethod {
    /// The instance this method is bound to.
    pub receiver: Gc<Object>,

    /// The actual method to run.
    pub closure: Gc<Object>,
}

impl BoundMethod {
    /// Creates a new bound method running `closure` for the instance `receiver`.
    pub fn new(receiver: Gc<Object>, closure: Gc<Object>) -> Self {
        Self { receiver, closure }
    }
}
