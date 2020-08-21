use super::rle::RLE;
use crate::vm::{instruction::*, value::Value};

#[derive(Debug, Default, Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: RLE<u64>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write(&mut self, instr: OpCode, line: u64) {
        self.code.push(instr.into());
        self.lines.push(line);
    }

    pub fn write_byte(&mut self, byte: u8, line: u64) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn write_index(&mut self, op_code: OpCode, index: u8, line: u64) {
        self.code.push(op_code.into());
        self.code.push(index);
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, constant: Value) -> u8 {
        self.constants.push(constant);
        let index = self.constants.len() - 1;
        if index > std::u8::MAX as usize {
            panic!("cannot have this many constants, yet...");
        }
        index as u8
    }

    pub fn read_constant(&self, index: usize) -> Option<&Value> {
        if let Some(i) = self.code.get(index + 1) {
            self.constants.get(*i as usize)
        } else {
            None
        }
    }
}
