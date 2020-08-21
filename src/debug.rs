use crate::compiler::chunk::Chunk;
use crate::vm::{instruction::OpCode, value::Value};

// Set to true if the stack should be shown on each instruction.
pub const TRACE_EXECUTION_STACK: bool = true;

// Set to true if each instruction executed should be shown.
pub const TRACE_EXECUTION_INSTR: bool = true;

// Set to true to trigger the GC when adding any new object.
pub const STRESS_GC: bool = false;

// Set to true to log the allocations and sweeping in the GC.
pub const LOG_GC: bool = false;

pub const LOG_COMPILER: bool = false;

pub const LOG_COMPILED_CODE: bool = true;

pub const LOG_OBJECT: bool = false;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    println!("[CODE] {:?}", chunk.code);

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

pub fn disassemble_instruction(chunk: &Chunk, index: usize) -> (String, usize) {
    let op_code = chunk.code.get(index).unwrap();
    let op_code = OpCode::from(op_code);
    let mut offset = index + 1;

    let constant_instruction = || {
        let idx = *chunk.code.get(index + 1).unwrap() as usize;
        let constant = chunk.read_constant(index).unwrap();
        match constant {
            Value::Object(object) => {
                format!("[const index] {}\t[variable] {:?}", idx, object.get())
            }
            _ => format!("[value] {}", constant),
        }
    };

    let byte_instruction = || {
        let byte = chunk.code.get(index + 1).unwrap();
        format!("[slot] {}", byte)
    };

    let jump_instruction = |sign: i64| {
        let byte0 = chunk.code.get(index + 1).unwrap().clone() as i64;
        let byte1 = chunk.code.get(index + 2).unwrap().clone() as i64;
        let jump = (byte0 << 8) | byte1;
        format!(
            "[JUMP] {:+} ({})",
            sign * jump,
            index as i64 + 3 + sign * jump
        )
    };

    let (text, bytes) = match op_code {
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
        | OpCode::CloseUpvalue
        | OpCode::Pop => ("".to_owned(), 1),
        OpCode::Constant | OpCode::DefineGlobal | OpCode::GetGlobal | OpCode::SetGlobal => {
            (constant_instruction(), 2)
        }
        OpCode::GetLocal
        | OpCode::SetLocal
        | OpCode::GetUpvalue
        | OpCode::SetUpvalue
        | OpCode::Call => (byte_instruction(), 2),
        OpCode::JumpIfFalse | OpCode::Jump => (jump_instruction(1), 3),
        OpCode::Loop => (jump_instruction(-1), 3),
        OpCode::Closure => {
            let constant = *chunk.code.get(offset).unwrap();
            offset += 1;
            let constant = chunk.constants.get(constant as usize).unwrap();
            let function = constant.as_function();

            let mut text = match constant {
                Value::Object(object) => {
                    format!("[index] {}\t[variable] {:?}", offset - 1, object.get())
                }
                _ => format!("[value] {}", constant),
            };

            for _i in 0..function.num_upvalues {
                let is_local = chunk.code.get(offset).unwrap();
                offset += 1;
                let upvalue_index = chunk.code.get(offset).unwrap();
                offset += 1;

                let is_local = if *is_local == 1 { "local" } else { "upvalue" };
                text += format!("\n{:<15}{} {} ", "|", is_local, upvalue_index).as_str();
            }

            (text, offset - index)
        }
    };

    (format!("{:<15}{}", op_code.name(), text), bytes)
}
