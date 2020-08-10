use crate::chunk::Chunk;
use crate::instruction::OpCode;
use crate::value::Value;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut instruction = 0;
    let mut offset = 0;
    while offset < chunk.code.len() {
        let current_line = chunk.lines.get(instruction).unwrap();
        let line = if instruction > 0 && chunk.lines.get(instruction - 1).unwrap() == current_line {
            "   |".to_owned()
        } else {
            format!("{:4}", current_line)
        };

        let (text, bytes) = disassemble_instruction(chunk, offset);

        println!("{:04} {} {}", offset, line, text);

        offset += bytes;
        instruction += 1;
    }
}

pub fn disassemble_instruction_i<'a>(
    chunk: &'a Chunk,
    op_code: u8,
    ip: &mut impl Iterator<Item = &'a u8>,
) -> (String, usize) {
    let op_code = OpCode::from(op_code);

    match op_code {
        OpCode::Return
        | OpCode::Negate
        | OpCode::Add
        | OpCode::Subtract
        | OpCode::Multiply
        | OpCode::Divide
        | OpCode::Nil
        | OpCode::True
        | OpCode::False
        | OpCode::Not
        | OpCode::Equal
        | OpCode::Greater
        | OpCode::Less => (format!("{}", op_code.name()), 1),
        OpCode::Constant => {
            let constant = chunk.read_constant_iter(ip).unwrap();
            match constant {
                Value::String(index) => (format!("{} String index: {}", op_code.name(), index), 2),
                _ => (format!("{} {}", op_code.name(), constant), 2),
            }
        }
        OpCode::ConstantLong => {
            let constant = chunk.read_constant_long_iter(ip).unwrap();
            (format!("{} {}", op_code.name(), constant), 4)
        }
    }
}

pub fn disassemble_instruction(chunk: &Chunk, index: usize) -> (String, usize) {
    let op_code = chunk.code.get(index).unwrap();
    let op_code = OpCode::from(op_code);

    match op_code {
        OpCode::Return
        | OpCode::Negate
        | OpCode::Add
        | OpCode::Subtract
        | OpCode::Multiply
        | OpCode::Divide
        | OpCode::Nil
        | OpCode::True
        | OpCode::False
        | OpCode::Not
        | OpCode::Equal
        | OpCode::Greater
        | OpCode::Less => (format!("{}", op_code.name()), 1),
        OpCode::Constant => {
            let constant = chunk.read_constant(index).unwrap();
            match constant {
                Value::String(index) => (format!("{} String index: {}", op_code.name(), index), 2),
                _ => (format!("{} {}", op_code.name(), constant), 2),
            }
        }
        OpCode::ConstantLong => {
            let constant = chunk.read_constant_long(index).unwrap();
            (format!("{} {}", op_code.name(), constant), 4)
        }
    }
}
