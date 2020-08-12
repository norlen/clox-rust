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
    options: VMOptions,
    stack: Vec<Value>,
    string_cache: StringCache,
    globals: HashMap<String, Value>,
}

pub struct VMOptions {
    pub trace_execution: bool,
}

impl VM {
    pub fn new(options: VMOptions) -> Self {
        VM {
            options,
            stack: Vec::new(),
            string_cache: StringCache::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let compiler = Compiler::new(source, &mut self.string_cache);
        let chunk = compiler.compile()?;
        self.run(&chunk, &chunk.code)
    }

    pub fn interpret_chunk(&mut self, chunk: &Chunk) -> Result<()> {
        self.run(chunk, &chunk.code)
    }

    fn run(&mut self, chunk: &Chunk, code: &[u8]) -> Result<()> {
        let mut ip = code.iter();
        while let Some(instruction) = ip.next() {
            if self.options.trace_execution {
                if self.stack.len() > 0 {
                    println!("-- stack --");
                    for val in self.stack.iter() {
                        match val {
                            Value::String(index) => {
                                let cached = self.string_cache.get(*index).unwrap();
                                print!("  [{}] ", cached)
                            },
                            _ => print!("  [{}] ", val),
                        };
                    }
                    println!("\n-- stack end --");
                }
                let (text, _offset) =
                    debug::disassemble_instruction_i(&chunk, &self.string_cache, *instruction, &mut ip.clone());
                println!("[Instruction] {}\n", text);
            }

            match OpCode::from(instruction) {
                OpCode::Return => break,
                OpCode::Constant => {
                    let constant = self.read_constant(&chunk, &mut ip).unwrap();
                    self.stack.push(constant.clone());
                }
                // OpCode::ConstantLong => {
                //     let constant = self.read_constant_long(&chunk, &mut ip).unwrap();
                //     self.stack.push(constant.clone());
                // }
                OpCode::Nil => {
                    self.stack.push(Value::Nil);
                }
                OpCode::True => {
                    self.stack.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.stack.push(Value::Bool(false));
                }
                OpCode::Equal => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let result = Value::equals(lhs, rhs).ok_or_else(|| {
                        VMError::TypeError("cannot compare these values".to_owned())
                    })?;
                    self.stack.push(Value::Bool(result));
                }
                OpCode::Greater => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.stack.push(Value::Bool(lhs > rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Less => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.stack.push(Value::Bool(lhs < rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Not => {
                    let value = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let value = match value {
                        Value::Bool(value) => Ok(!value),
                        Value::Nil => Ok(true),
                        _ => Err(VMError::TypeError("expected a nil or bool".to_owned())),
                    }?;
                    self.stack.push(Value::Bool(value));
                }
                OpCode::Negate => {
                    let value = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match value {
                        Value::Number(v) => {
                            self.stack.push(Value::Number(-v));
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Add => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.stack.push(Value::Number(lhs + rhs))
                        }
                        (Value::String(lhs), Value::String(rhs)) => {
                            let lhs = self.string_cache.get(lhs).ok_or(VMError::RuntimeError)?;
                            let rhs = self.string_cache.get(rhs).ok_or(VMError::RuntimeError)?;
                            let new = lhs.to_owned() + rhs;
                            let new = self.string_cache.cache(new);
                            self.stack.push(Value::String(new));
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Subtract => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.stack.push(Value::Number(lhs - rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Multiply => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.stack.push(Value::Number(lhs * rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Divide => {
                    let rhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.stack.push(Value::Number(lhs / rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Print => {
                    let value = self.stack.pop().ok_or(VMError::RuntimeError)?;
                    match value {
                        Value::String(index) => {
                            let cached = self.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            println!("{}", cached);
                        },
                        _ => println!("{}", value),
                    };
                }
                OpCode::Pop => {
                    // We don't care about the value here, used in expression statements.
                    self.stack.pop();
                },
                OpCode::DefineGlobal => {
                    if let Some(Value::String(index)) = self.read_constant(&chunk, &mut ip) {
                        let constant_identifier = self.string_cache.get(*index).unwrap();
                        let value = self.stack.get(self.stack.len()-1).unwrap();
                        self.globals.insert(constant_identifier.to_owned(), value.clone());
                        self.stack.pop();
                    } else {
                        panic!();
                    }
                }
                OpCode::GetGlobal => {
                    if let Some(Value::String(index)) = self.read_constant(&chunk, &mut ip) {
                        let constant_identifier = self.string_cache.get(*index).unwrap();
                        let value = self.globals.get(constant_identifier).ok_or(VMError::RuntimeError)?;
                        self.stack.push(value.clone());
                    } else {
                        panic!();
                    }
                }
                OpCode::SetGlobal => {
                    if let Some(Value::String(index)) = self.read_constant(&chunk, &mut ip) {
                        let constant_identifer = self.string_cache.get(*index).unwrap();
                        if self.globals.contains_key(constant_identifer) {
                            let value = self.stack.last().ok_or(VMError::RuntimeError)?;
                            self.globals.insert(constant_identifer.to_owned(), value.clone());
                        } else {
                            return Err(VMError::UndefinedVariable(constant_identifer.to_owned()));
                        }
                    } else {
                        panic!();
                    }
                }
                OpCode::GetLocal => {
                    let index = ip.next().ok_or(VMError::RuntimeError)?.clone();
                    let value = self.stack.get(index as usize).ok_or(VMError::RuntimeError)?.clone();
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let index = ip.next().ok_or(VMError::RuntimeError)?.clone();
                    let value = self.stack.last().ok_or(VMError::RuntimeError)?.clone();
                    self.stack.insert(index as usize, value);
                }
            }
        }
        Ok(())
    }

    fn read_constant<'src>(
        &self,
        chunk: &'src Chunk,
        ip: &mut impl Iterator<Item = &'src u8>,
    ) -> Option<&'src Value> {
        let index = *ip.next().unwrap() as usize;
        chunk.constants.get(index)
    }

    // fn read_constant_long<'src>(
    //     &self,
    //     chunk: &'src Chunk,
    //     ip: &mut impl Iterator<Item = &'src u8>,
    // ) -> Option<&'src Value> {
    //     let b2 = *ip.next().unwrap();
    //     let b1 = *ip.next().unwrap();
    //     let b0 = *ip.next().unwrap();
    //     let index = chunk::combine_index(b2, b1, b0);
    //     chunk.constants.get(index)
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    const VM_OPTIONS: VMOptions = VMOptions {
        trace_execution: true,
    };

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

        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret_chunk(&chunk).is_ok());
    }

    #[test]
    fn vm_math0() {
        let source = "(-1 + 2) * 3 - -4;";
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_math1() {
        let source = "!(5 - 4 > 3 * 2 == !nil);";
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_string0() {
        let source = "\"st\" + \"ri\" + \"ng\";";
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_string_interning() {
        let source = "\"hello\" + \"hello\" + \"hello\";";
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_print() {
        let source = "print 3 + (4 * 3) * (1 + (2 + 3));";
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_constants() {
        let source = r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;
        "#;
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }

    #[test]
    fn vm_locals_simple() {
        let source = "{ var a = 2; }";
        let mut vm = VM::new(VM_OPTIONS);
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
        let mut vm = VM::new(VM_OPTIONS);
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
        let mut vm = VM::new(VM_OPTIONS);
        assert!(vm.interpret(source).is_ok());
    }
}
