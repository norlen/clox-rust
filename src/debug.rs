use crate::chunk::Chunk;
use crate::instruction::OpCode;
use crate::value::Value;
use crate::string_cache::StringCache;

pub fn disassemble_chunk(chunk: &Chunk, strings: &StringCache, name: &str) {
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

        let (text, bytes) = disassemble_instruction(chunk, strings, offset);

        println!("{:04} {} {}", offset, line, text);

        offset += bytes;
        instruction += 1;
    }
}

pub fn disassemble_instruction_i<'a>(
    chunk: &'a Chunk,
    strings: &'a StringCache,
    op_code: u8,
    ip: &mut impl Iterator<Item = &'a u8>,
) -> (String, usize) {
    let op_code = OpCode::from(op_code);

    let constant_instruction = || {
        let constant = chunk.read_constant_iter(ip).unwrap();
        match constant {
            Value::String(index) => {
                let cached = strings.get(*index).unwrap();
                (format!("{}\t[index] {}\t[contains] {}", op_code.name(), index, cached), 2)
            },
            _ => (format!("{}\t[value]{}", op_code.name(), constant), 2),
        }
    };

    let byte_instruction = || ("".to_owned(), 1337);

    match op_code {
        OpCode::SetLocal | OpCode::GetLocal => {
            let byte = ip.next().unwrap();
            (format!("{}\t[slot] {}", op_code.name(), byte), 2)
        },
        _ => get_string(op_code, constant_instruction, byte_instruction)
    }
}

pub fn disassemble_instruction(chunk: &Chunk, strings: &StringCache, index: usize) -> (String, usize) {
    let op_code = chunk.code.get(index).unwrap();
    let op_code = OpCode::from(op_code);

    let constant_instruction = || {
        let constant = chunk.read_constant(index).unwrap();
        match constant {
            Value::String(index) => {
                let cached = strings.get(*index).unwrap();
                (format!("{}\t[index] {}\t[contains] {}", op_code.name(), index, cached), 2)
            },
            _ => (format!("{}\t[value]{}", op_code.name(), constant), 2),
        }
    };

    let byte_instruction = || {
        let byte = chunk.code.get(index + 1).unwrap();
        (format!("{}\t[slot] {}", op_code.name(), byte), 2)
    };

    get_string(op_code, constant_instruction, byte_instruction)
}

fn get_string<F, G>(op_code: OpCode, constant_instruction: F, byte_instruction: G) -> (String, usize)
where
    F: FnOnce() -> (String, usize),
    G: FnOnce() -> (String, usize),
{
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
        | OpCode::Less
        | OpCode::Print
        | OpCode::Pop => (format!("{}", op_code.name()), 1),
        OpCode::Constant
        | OpCode::DefineGlobal
        | OpCode::GetGlobal
        | OpCode::SetGlobal => constant_instruction(),
        | OpCode::GetLocal
        | OpCode::SetLocal => byte_instruction(),
        
        // OpCode::ConstantLong => {
        //     let constant = chunk.read_constant_long_iter(ip).unwrap();
        //     (format!("{} {}", op_code.name(), constant), 4)
        // }
    }
}
