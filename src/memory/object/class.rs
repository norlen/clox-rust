use super::{Closure, Gc};
use crate::vm::value::Value;
use std::collections::HashMap;
use std::fmt;

/// A class declaration containg the name and all it's method.
#[derive(Debug, Clone)]
pub struct Class {
    /// Name of the class.
    pub name: Gc<String>,

    /// Methods this class contains.
    pub methods: HashMap<String, Gc<Closure>>,
}

impl Class {
    /// Creates a new class with `name`. Requires the name object to be a
    /// garbage collected string.
    pub fn new(name: Gc<String>) -> Self {
        Class {
            name,
            methods: HashMap::new(),
        }
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.name.as_ref())
    }
}

/// An instance of a class.
#[derive(Debug, Clone)]
pub struct Instance {
    /// The class this instance is created from.
    pub class: Gc<Class>,
    
    /// Values for all the fields set.
    pub fields: HashMap<String, Value>,
}

impl Instance {
    /// Creates a new class instance, `class` must be a valid garbage collected class.
    pub fn new(class: Gc<Class>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

impl fmt::Display for Instance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance of {}>", self.class.as_ref())
    }
}

/// A method bound to a specific instance.
#[derive(Debug, Clone)]
pub struct BoundMethod {
    /// The instance this method is bound to.
    pub receiver: Gc<Instance>,

    /// The actual method to run.
    pub closure: Gc<Closure>,
}

impl BoundMethod {
    /// Creates a new bound method running `closure` for the instance `receiver`.
    pub fn new(receiver: Gc<Instance>, closure: Gc<Closure>) -> Self {
        Self { receiver, closure }
    }
}

impl fmt::Display for BoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<method {} of {}>", self.closure.as_ref(), self.receiver.as_ref())
    }
}
