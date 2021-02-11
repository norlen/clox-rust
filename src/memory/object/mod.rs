use colored::*;
use std::fmt::{self, Debug};

use super::{Gc, GC};
use crate::debug::LOG_OBJECT;

mod class;
mod closure;
mod function;
mod native_fn;
mod upvalue;
mod instance;

pub use class::Class;
pub use closure::Closure;
pub use function::Function;
pub use upvalue::Upvalue;
pub use native_fn::{NativeFn, NativeFunction};
pub use instance::Instance;

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Function(Function),
    Native(NativeFn),
    Closure(Closure),
    Upvalue(Upvalue),
    Class(Class),
    Instance(Instance),
}

impl Drop for Object {
    fn drop(&mut self) {
        if LOG_OBJECT {
            println!("{}\tDROP Value: {:?}", "[OBJECT]".purple(), self);
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(v) => write!(f, "{}", v.as_str()),
            Object::Function(v) => write!(f, "<fn {}>", v.function_name()),
            Object::Native(v) => write!(f, "<native fn {}>", v.function_name()),
            Object::Closure(v) => {
                write!(f, "<closure {}>", v.function.as_function().function_name())
            }
            Object::Upvalue(_v) => write!(f, "upvalue"),
            Object::Class(v) => write!(f, "<class {}>", v.name.as_string()),
            Object::Instance(v) => write!(f, "<instance of {}>", v.class.as_class().name.as_string()),
        }
    }
}

impl Object {
    pub fn as_string(&self) -> &String {
        match self {
            Object::String(string) => string,
            _ => panic!("Expected string"),
        }
    }

    pub fn as_function(&self) -> &Function {
        match self {
            Object::Function(function) => function,
            _ => panic!("Expected function"),
        }
    }

    // pub fn as_native(&self) -> &NativeFn {
    //     match self {
    //         Object::Native(native) => native,
    //         _ => panic!("Expected native function"),
    //     }
    // }

    pub fn as_closure(&self) -> &Closure {
        match self {
            Object::Closure(closure) => closure,
            _ => panic!("Expected closure"),
        }
    }

    pub fn as_upvalue(&self) -> &Upvalue {
        match self {
            Object::Upvalue(upvalue) => upvalue,
            _ => panic!("Expected upvalue"),
        }
    }

    pub fn as_closure_mut(&mut self) -> &mut Closure {
        match self {
            Object::Closure(closure) => closure,
            _ => panic!("Expected closure"),
        }
    }

    pub fn as_upvalue_mut(&mut self) -> &mut Upvalue {
        match self {
            Object::Upvalue(upvalue) => upvalue,
            _ => panic!("Expected upvalue"),
        }
    }

    pub fn as_class(&self) -> &Class {
        match self {
            Object::Class(class) => class,
            _ => panic!("Expected class"),
        }
    }

    pub fn as_instance(&self) -> &Instance {
        match self {
            Object::Instance(instance) => instance,
            _ => panic!("Expected instance"),
        }
    }

    pub fn as_instance_mut(&mut self) -> &mut Instance {
        match self {
            Object::Instance(instance) => instance,
            _ => panic!("Expected instance"),
        }
    }
}



