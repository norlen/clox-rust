use crate::instruction::*;
use crate::rle::RLE;
use crate::value::Value;

#[derive(Default, Clone)]
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

    // pub fn add_constant_long(&mut self, constant: Value, line: u64) {
    //     self.constants.push(constant);
    //     self.lines.push(line);
    //     self.code.push(OpCode::ConstantLong.into());

    //     let index = split_index(self.constants.len() - 1);
    //     self.code.push(index.0);
    //     self.code.push(index.1);
    //     self.code.push(index.2);
    // }

    pub fn read_constant_iter<'src>(
        &self,
        ip: &mut impl Iterator<Item = &'src u8>,
    ) -> Option<&Value> {
        if let Some(i) = ip.next() {
            self.constants.get(*i as usize)
        } else {
            None
        }
    }

    // pub fn read_constant_long_iter<'src>(
    //     &self,
    //     ip: &mut impl Iterator<Item = &'src u8>,
    // ) -> Option<&Value> {
    //     let b2 = *ip.next()?;
    //     let b1 = *ip.next()?;
    //     let b0 = *ip.next()?;
    //     let constant_index = combine_index(b2, b1, b0);
    //     self.constants.get(constant_index)
    // }

    pub fn read_constant(&self, index: usize) -> Option<&Value> {
        if let Some(i) = self.code.get(index + 1) {
            self.constants.get(*i as usize)
        } else {
            None
        }
    }

    // pub fn read_constant_long(&self, index: usize) -> Option<&Value> {
    //     let b2 = *self.code.get(index + 1)?;
    //     let b1 = *self.code.get(index + 2)?;
    //     let b0 = *self.code.get(index + 3)?;
    //     let constant_index = combine_index(b2, b1, b0);
    //     self.constants.get(constant_index)
    // }
}

// pub fn split_index(index: usize) -> (u8, u8, u8) {
//     let b2 = ((index >> 16) & 0xFF) as u8;
//     let b1 = ((index >> 8) & 0xFF) as u8;
//     let b0 = ((index >> 0) & 0xFF) as u8;
//     (b2, b1, b0)
// }

// pub fn combine_index(b2: u8, b1: u8, b0: u8) -> usize {
//     (b2 as usize) << 16 | (b1 as usize) << 8 | b0 as usize
// }
