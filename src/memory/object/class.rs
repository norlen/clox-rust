use super::Gc;
use super::Object;

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Gc<Object>, // String.
}

impl Class {
    fn new(name: Gc<Object>) -> Self {
        Class {
            name,
        }
    }
}
