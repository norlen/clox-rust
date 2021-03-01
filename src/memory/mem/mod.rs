#![allow(dead_code)]

mod arch;
mod gc;
mod objects;
mod trace;
mod util;

use gc::Gc;

use crate::compiler::chunk::Chunk;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Gc<String>>,
    pub arity: i64,
    pub chunk: Chunk,
    pub num_upvalues: usize,
}

pub struct Root<T> {
    data: Gc<T>,
}
