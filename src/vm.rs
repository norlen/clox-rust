use crate::chunk::Chunk;
use crate::compiler::{CompileError, Compiler};
use crate::debug;
use crate::instruction::OpCode;
use crate::string_cache::StringCache;
use crate::value::Value;
use thiserror::Error;
use std::collections::HashMap;

pub type Result<T> = std::result::Result<T, VMError>;

#[derive(Debug, Error)]
pub enum VMError {
    #[error("Compile error")]
    CompileError(#[from] CompileError),

    #[error("Runtime error")]
    RuntimeError,

    #[error("Type mismatch: {}", .0)]
    TypeError(String),

    #[error("Undefined variable: {}", .0)]
    UndefinedVariable(String),
}

pub struct VM {
    stack: Vec<Value>,
    string_cache: StringCache,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::new(),
            string_cache: StringCache::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let compiler = Compiler::new(source, &mut self.string_cache);
        let chunk = compiler.compile()?;
        self.interpret_chunk(chunk)
    }

    pub fn interpret_chunk(&mut self, chunk: Chunk) -> Result<()> {
        let mut executor = Execuction::new(self, chunk);
        executor.run()
    }
}


struct Execuction<'vm> {
    vm: &'vm mut VM,
    chunk: Chunk,
    ip: usize,
    trace_execution: bool,
}

impl<'vm> Execuction<'vm> {
    fn new(vm: &'vm mut VM, chunk: Chunk) -> Self {
        Self { vm, chunk, ip: 0, trace_execution: true }
    }

    fn next_instruction(&mut self) -> Result<u8> {
        self.ip += 1;
        self.chunk.code.get(self.ip - 1).copied().ok_or(VMError::RuntimeError)
    }

    fn peek_instruction(&mut self, offset: i64) -> Result<u8> {
        let index = self.chunk.code.len() as i64 - offset;
        assert!(self.chunk.code.len() < std::i64::MAX as usize);
        assert!(index > 0);
        self.chunk.code.get(index as usize).copied().ok_or(VMError::RuntimeError)
    }
    
    fn print_debug(&self) {
        let r = debug::disassemble_instruction(&self.chunk, &self.vm.string_cache, self.ip - 1);
        println!("\n[Instruction]\t{:04}\t{}", self.ip-1, r.0);
        if self.vm.stack.len() > 0 {
            let mut stack_str = Vec::new();
            for val in self.vm.stack.iter() {
                let ss = match val {
                    Value::String(index) => {
                        let cached = self.vm.string_cache.get(*index).unwrap();
                        format!(" [{}]", cached)
                    },
                    _ => format!(" [{}]", val),
                };
                stack_str.push(ss);
            }
            let stack = stack_str.iter().fold(String::new(), |acc, s| acc + s);
            println!("[STACK]\t\t{}", stack.trim_start());
        }
    }

    fn run(&mut self) -> Result<()> {
        while self.ip < self.chunk.code.len() {
            let instruction = self.next_instruction()?;
            let instruction = OpCode::from(instruction);

            if self.trace_execution {
                self.print_debug();
            }

            match instruction {
                OpCode::Return => break,
                OpCode::Constant => {
                    let constant = self.next_instruction_as_constant()?;
                    let constant = constant.clone();
                    self.vm.stack.push(constant);
                }
                // OpCode::ConstantLong => {}
                OpCode::Nil => {
                    self.vm.stack.push(Value::Nil);
                }
                OpCode::True => {
                    self.vm.stack.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.vm.stack.push(Value::Bool(false));
                }
                OpCode::Equal => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let result = Value::equals(lhs, rhs).ok_or_else(|| {
                        VMError::TypeError("cannot compare these values".to_owned())
                    })?;
                    self.vm.stack.push(Value::Bool(result));
                }
                OpCode::Greater => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.vm.stack.push(Value::Bool(lhs > rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Less => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.vm.stack.push(Value::Bool(lhs < rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Not => {
                    let value = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let value = match value {
                        Value::Bool(value) => Ok(!value),
                        Value::Nil => Ok(true),
                        _ => Err(VMError::TypeError("expected a nil or bool".to_owned())),
                    }?;
                    self.vm.stack.push(Value::Bool(value));
                }
                OpCode::Negate => {
                    let value = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match value {
                        Value::Number(v) => {
                            self.vm.stack.push(Value::Number(-v));
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Add => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.vm.stack.push(Value::Number(lhs + rhs))
                        }
                        (Value::String(lhs), Value::String(rhs)) => {
                            let lhs = self.vm.string_cache.get(lhs).ok_or(VMError::RuntimeError)?;
                            let rhs = self.vm.string_cache.get(rhs).ok_or(VMError::RuntimeError)?;
                            let new = lhs.to_owned() + rhs;
                            let new = self.vm.string_cache.cache(new);
                            self.vm.stack.push(Value::String(new));
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Subtract => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.vm.stack.push(Value::Number(lhs - rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Multiply => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.vm.stack.push(Value::Number(lhs * rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Divide => {
                    let rhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.vm.stack.push(Value::Number(lhs / rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Print => {
                    let value = self.vm.stack.pop().ok_or(VMError::RuntimeError)?;
                    match value {
                        Value::String(index) => {
                            let cached = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            println!("{}", cached);
                        },
                        _ => println!("{}", value),
                    };
                }
                OpCode::Pop => {
                    // We don't care about the value here, used in expression statements.
                    self.vm.stack.pop();
                },
                OpCode::DefineGlobal => {
                    let next_instruction = self.next_instruction_as_constant()?;
                    match next_instruction.clone() {
                        Value::String(index) => {
                            let constant_identifier = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            let value = self.vm.stack.get(self.vm.stack.len()-1).ok_or(VMError::RuntimeError)?;
                            self.vm.globals.insert(constant_identifier.to_owned(), value.clone());
                            self.vm.stack.pop();
                        }
                        _ => panic!(),
                    }
                }
                OpCode::GetGlobal => {
                    let next_instruction = self.next_instruction_as_constant()?;
                    match next_instruction.clone() {
                        Value::String(index) => {
                            let constant_identifier = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            let value = self.vm.globals.get(constant_identifier).ok_or(VMError::RuntimeError)?;
                            self.vm.stack.push(value.clone());
                        },
                        _ => panic!(),
                    }
                }
                OpCode::SetGlobal => {
                    let next_instruction = self.next_instruction_as_constant()?;
                    match next_instruction.clone() {
                        Value::String(index) => {
                            let constant_identifer = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            if self.vm.globals.contains_key(constant_identifer) {
                                let value = self.vm.stack.last().ok_or(VMError::RuntimeError)?;
                                self.vm.globals.insert(constant_identifer.to_owned(), value.clone());
                            } else {
                                return Err(VMError::UndefinedVariable(constant_identifer.to_owned()));
                            }
                        },
                        _ => panic!(),
                    }
                }
                OpCode::GetLocal => {
                    let index = self.next_instruction()?;
                    let value = self.vm.stack.get(index as usize).ok_or(VMError::RuntimeError)?.clone();
                    self.vm.stack.push(value);
                }
                OpCode::SetLocal => {
                    let index = self.next_instruction()?;
                    let value = self.vm.stack.last().ok_or(VMError::RuntimeError)?.clone();
                    self.vm.stack.insert(index as usize, value);
                }
                OpCode::JumpIfFalse => {
                    let offset = self.next_instruction_as_jump()?;
                    let condition_value = match self.vm.stack.last().ok_or(VMError::RuntimeError)? {
                        Value::Nil => false,
                        Value::Bool(val) => *val,
                        _ => panic!(),
                    };
                    if !condition_value {
                        self.ip = (self.ip as i64 + offset) as usize;
                    }
                }
            }
        }
        Ok(())
    }

    // fn peek_constant(&mut self) -> 

    fn next_instruction_as_constant(&mut self) -> Result<&Value> {
        let index = self.next_instruction()?;
        self.chunk.constants.get(index as usize).ok_or(VMError::RuntimeError)
    }

    fn next_instruction_as_jump(&mut self) -> Result<i64> {
        let b0 = self.next_instruction()? as i64;
        let b1 = self.next_instruction()? as i64;
        Ok(b0 << 8 | b1)
    }

    // fn next_instruction_as_constant_long(&mut self) -> Result<&Value> {
    //     let b2 = self.next_instruction()?;
    //     let b1 = self.next_instruction()?;
    //     let b0 = self.next_instruction()?;
    //     let index = chunk::combine_index(b2, b1, b0);
    //     self.chunk.constants.get(index)
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_raw_instructions() {
        use crate::instruction;
        use crate::value::Value;

        let add_constant = |chunk: &mut Chunk, value| {
            let index = chunk.add_constant(Value::Number(value));
            chunk.write_index(OpCode::Constant, index, 0);
        };

        let mut chunk = Chunk::new();
        add_constant(&mut chunk, 1.2);
        add_constant(&mut chunk, 3.4);
        chunk.write(instruction::OpCode::Add, 0);
        add_constant(&mut chunk, 5.6);
        chunk.write(instruction::OpCode::Divide, 0);
        chunk.write(instruction::OpCode::Negate, 0);
        // chunk.add_constant_long(Value::Number(2.5), 0);
        chunk.write(instruction::OpCode::Return, 0);

        let cache = StringCache::new();
        debug::disassemble_chunk(&chunk, &cache, "test chunk");

        let mut vm = VM::new();
        assert!(vm.interpret_chunk(chunk).is_ok());
    }

    #[test]
    fn vm_math0() {
        let source = "(-1 + 2) * 3 - -4;";
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_math1() {
        let source = "!(5 - 4 > 3 * 2 == !nil);";
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_string0() {
        let source = "\"st\" + \"ri\" + \"ng\";";
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_string_interning() {
        let source = "\"hello\" + \"hello\" + \"hello\";";
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_print() {
        let source = "print 3 + (4 * 3) * (1 + (2 + 3));";
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_constants() {
        let source = r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;
        "#;
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_locals_simple() {
        let source = "{ var a = 2; }";
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_locals1() {
        let source = r#"
        {
            var a = 1;
            var b = a;
            b = b + 1;
            var c = a + b;
            a = 5;
            b = 10;
            c = a + b;
        }
        "#;
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_locals2() {
        let source = r#"
        {
            var a = 1;
            {
                var b = 2;
                {
                    var c = 3;
                    {
                        var d = 4;
                    }
                }
            }
        }
        "#;
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_if_statement() {
        let source = r#"
        var a = 1;
        var b = 1;
        if (a == 1) {
            a = 2;
        }
        if (b == 2) {
            b = 3;
        }
        print a;
        print b;
        "#;
        let mut vm = VM::new();
        assert!(vm.interpret(source).is_ok());
    }
}
