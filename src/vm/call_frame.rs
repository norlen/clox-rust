use super::{value::Value, Result, VMError};
use crate::memory::{Closure, Function};

#[derive(Clone)]
pub struct CallFrame {
    pub(super) closure: Closure,
    pub(super) ip: usize,
    pub(super) stack_base: usize,
}

impl CallFrame {
    pub(super) fn new(closure: Closure, stack_base: usize) -> Self {
        Self {
            closure,
            ip: 0,
            stack_base,
        }
    }

    pub(super) fn next_instruction(&mut self) -> Result<u8> {
        self.ip += 1;
        self.closure
            .function
            .as_function()
            .chunk
            .code
            .get(self.ip - 1)
            .copied()
            .ok_or(VMError::RuntimeError)
    }

    pub(super) fn next_instruction_as_constant(&mut self) -> Result<&Value> {
        let index = self.next_instruction()? as usize;
        self.constants().get(index).ok_or(VMError::RuntimeError)
    }

    pub(super) fn next_instruction_as_jump(&mut self) -> Result<usize> {
        let b0 = self.next_instruction()? as usize;
        let b1 = self.next_instruction()? as usize;
        Ok(b0 << 8 | b1)
    }

    pub(super) fn function(&self) -> &Function {
        self.closure.function.as_function()
    }

    pub(super) fn code(&self) -> &Vec<u8> {
        &self.closure.function.as_function().chunk.code
    }

    pub(super) fn constants(&self) -> &Vec<Value> {
        &self.closure.function.as_function().chunk.constants
    }
}
