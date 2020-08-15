use crate::compiler::{CompileError, Compiler};
use crate::debug;
use crate::instruction::OpCode;
use crate::string_cache::StringCache;
use crate::value::{Value, Function};
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
    string_cache: StringCache,
    globals: HashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            string_cache: StringCache::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<()> {
        let compiler = Compiler::new(source, &mut self.string_cache);
        let func = compiler.compile()?;
        self.interpret_function(func).unwrap();
        Ok(())
    }

    pub fn interpret_function(&mut self, func: Function) -> Result<()> {
        let mut executor = Execuction::new(self, func);
        if let Err(err) = executor.run() {
            // Stack trace.
            for frame in executor.call_frames.iter().rev() {
                let name = if frame.function.name == "".to_owned() {
                    "script"
                } else {
                    frame.function.name.as_str()
                };

                let instruction = frame.ip - 1;
                let line = frame.function.chunk.lines.get(instruction).copied().unwrap_or(1337);
                println!("[line {}] in {}", line, name);
            }
            return Err(err);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct CallFrame {
    function: Function,
    ip: usize,
    stack_base: usize,
}

impl CallFrame {
    fn new(function: Function, stack_base: usize) -> Self {
        Self {
            function,
            ip: 0,
            stack_base,
        }
    }

    fn next_instruction(&mut self) -> Result<u8> {
        self.ip += 1;
        self.function.chunk.code.get(self.ip - 1).copied().ok_or(VMError::RuntimeError)
    }

    fn peek_instruction(&mut self, offset: i64) -> Result<u8> {
        let index = self.function.chunk.code.len() as i64 - offset;
        assert!(self.function.chunk.code.len() < std::i64::MAX as usize);
        assert!(index > 0);
        self.function.chunk.code.get(index as usize).copied().ok_or(VMError::RuntimeError)
    }

    fn next_instruction_as_constant(&mut self) -> Result<&Value> {
        let index = self.next_instruction()?;
        self.function.chunk.constants.get(index as usize).ok_or(VMError::RuntimeError)
    }

    fn next_instruction_as_jump(&mut self) -> Result<usize> {
        let b0 = self.next_instruction()? as usize;
        let b1 = self.next_instruction()? as usize;
        Ok(b0 << 8 | b1)
    }
}

struct Execuction<'vm> {
    vm: &'vm mut VM,
    trace_execution: bool,
    stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
}

impl<'vm> Execuction<'vm> {
    fn new<'t>(vm: &'vm mut VM, function: Function) -> Self {
        Self {
            vm,
            trace_execution: true,
            stack: vec![Value::Function(function.clone())],
            call_frames: vec![CallFrame::new(function, 0)],
        }
    }

    fn run(&mut self) -> Result<()> {
        let mut frame = self.call_frames.pop().ok_or(VMError::RuntimeError)?;

        while frame.ip < frame.function.chunk.code.len() {
            let instruction = frame.next_instruction()?;
            let instruction = OpCode::from(instruction);

            if self.trace_execution {
                let r = debug::disassemble_instruction(&frame.function.chunk, &self.vm.string_cache, frame.ip - 1);
                if self.stack.len() > 0 {
                    let mut stack_str = Vec::new();
                    for val in self.stack.iter() {
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
                    println!("\n[STACK]\t\t{}", stack.trim_start());
                } else {
                    println!("\n[STACK]");
                }
                println!("[Instruction]\t{:04}\t{}", frame.ip-1, r.0);
            }

            match instruction {
                OpCode::Return => {
                    let result = self.stack.pop().ok_or(VMError::RuntimeError)?;

                    if self.call_frames.len() == 0 {
                        self.stack.pop();
                        return Ok(());
                    }

                    self.stack.truncate(frame.stack_base);
                    frame = self.call_frames.pop().unwrap();

                    self.stack.push(result);
                },
                OpCode::Constant => {
                    let constant = frame.next_instruction_as_constant()?;
                    let constant = constant.clone();
                    self.stack.push(constant);
                }
                // OpCode::ConstantLong => {}
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
                            let lhs = self.vm.string_cache.get(lhs).ok_or(VMError::RuntimeError)?;
                            let rhs = self.vm.string_cache.get(rhs).ok_or(VMError::RuntimeError)?;
                            let new = lhs.to_owned() + rhs;
                            let new = self.vm.string_cache.cache(new);
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
                            let cached = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
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
                    let next_instruction = frame.next_instruction_as_constant()?;
                    match next_instruction.clone() {
                        Value::String(index) => {
                            let constant_identifier = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            let value = self.stack.get(self.stack.len()-1).ok_or(VMError::RuntimeError)?;
                            self.vm.globals.insert(constant_identifier.to_owned(), value.clone());
                            self.stack.pop();
                        }
                        _ => panic!(),
                    }
                }
                OpCode::GetGlobal => {
                    let next_instruction = frame.next_instruction_as_constant()?;
                    match next_instruction.clone() {
                        Value::String(index) => {
                            let constant_identifier = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            let value = self.vm.globals.get(constant_identifier).ok_or(VMError::RuntimeError)?;
                            self.stack.push(value.clone());
                        },
                        _ => panic!(),
                    }
                }
                OpCode::SetGlobal => {
                    let next_instruction = frame.next_instruction_as_constant()?;
                    match next_instruction.clone() {
                        Value::String(index) => {
                            let constant_identifer = self.vm.string_cache.get(index).ok_or(VMError::RuntimeError)?;
                            if self.vm.globals.contains_key(constant_identifer) {
                                let value = self.stack.last().ok_or(VMError::RuntimeError)?;
                                self.vm.globals.insert(constant_identifer.to_owned(), value.clone());
                            } else {
                                return Err(VMError::UndefinedVariable(constant_identifer.to_owned()));
                            }
                        },
                        _ => panic!(),
                    }
                }
                OpCode::GetLocal => {
                    let index = frame.next_instruction()? as usize + frame.stack_base;
                    println!("index: {} | stack_base: {}", index, frame.stack_base);
                    // println!("call frames: {:?}", self.call_frames);
                    let value = self.stack.get(index).ok_or(VMError::RuntimeError)?.clone();
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let index = frame.next_instruction()?;
                    let value = self.stack.last().ok_or(VMError::RuntimeError)?.clone();
                    self.stack[index as usize + frame.stack_base] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = frame.next_instruction_as_jump()?;
                    let condition_value = match self.stack.last().ok_or(VMError::RuntimeError)? {
                        Value::Nil => false,
                        Value::Bool(val) => *val,
                        _ => panic!(),
                    };
                    if !condition_value {
                        frame.ip += offset;
                    }
                }
                OpCode::Jump => {
                    let offset = frame.next_instruction_as_jump()?;
                    frame.ip += offset;
                }
                OpCode::Loop => {
                    let offset = frame.next_instruction_as_jump()?;
                    frame.ip -= offset;
                }
                OpCode::Call => {
                    let arg_count = frame.next_instruction()? as usize;
                    let fun_index = self.stack.len() - arg_count - 1;
                    self.call_frames.push(frame);

                    let fun = self.stack.get(fun_index).ok_or(VMError::RuntimeError)?;
                    let fun = fun.clone();

                    self.call_value(fun, arg_count)?;
                    frame = self.call_frames.pop().unwrap();
                }
            }
        }
        Ok(())
    }

    fn call_value(&mut self, fun: Value, arg_count: usize) -> Result<()> {
        match fun {
            Value::Function(fun) => self.call(fun, arg_count),
            _ => Err(VMError::RuntimeError),
        }
    }
    
    fn call(&mut self, fun: Function, arg_count: usize) -> Result<()> {
        if arg_count != fun.arity as usize {
            panic!("Expected {} arguments but got {}", fun.arity, arg_count);
        }

        let call_frame = CallFrame::new(fun, self.stack.len() - arg_count - 1);
        self.call_frames.push(call_frame);

        if self.call_frames.len() > 255 {
            // We should have some limit at least.
            panic!("Stack overflow");
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vm_raw_instructions() {
        use crate::instruction;
        use crate::value::Value;
        use crate::chunk::Chunk;

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
        add_constant(&mut chunk, 2.5);
        chunk.write(instruction::OpCode::Return, 0);

        let cache = StringCache::new();
        debug::disassemble_chunk(&chunk, &cache, "test chunk");

        let fun = Function {
            arity: 0,
            name: "".to_owned(),
            chunk,
        };

        let mut vm = VM::new();
        assert!(vm.interpret_function(fun).is_ok());
    }

    #[test]
    fn vm_math0() {
        let source = "(-1 + 2) * 3 - -4;";
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_math1() {
        let source = "!(5 - 4 > 3 * 2 == !nil);";
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_string0() {
        let source = "\"st\" + \"ri\" + \"ng\";";
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_string_interning() {
        let source = "\"hello\" + \"hello\" + \"hello\";";
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_print() {
        let source = "print 3 + (4 * 3) * (1 + (2 + 3));";
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_constants() {
        let source = r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_locals_simple() {
        let source = "{ var a = 2; }";
        assert!(VM::new().interpret(source).is_ok());
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
        assert!(VM::new().interpret(source).is_ok());
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
        assert!(VM::new().interpret(source).is_ok());
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
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_if_else_statement() {
        let source = r#"
        var a = 1;
        var b = 1;
        if (a == 1) {
            a = 10;
        } else {
            a = 20;
        }
        if (b == 2) {
            b = 100;
        } else {
            b = 200;
        }
        print a; // expect: 10
        print b; // expect: 200
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_and_or() {
        let source = r#"
        var a = true and false; // false
        var b = true or false;  // true
        var c = a and b;        // false
        var d = a or b;         // true
        print "should be false";
        print a;
        print "should be true";
        print b;
        print "should be false";
        print c;
        print "should be true";
        print d;
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_while() {
        let source = r#"
        var a = 0;
        while (a < 10) {
            a = a + 1;
        }
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_for() {
        let source = r#"
        for (var i = 0; i < 10; i = i + 1) {
            print i;
        }
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_fun_simple() {
        let source = r#"
        fun hello() {}
        fun hello2(a) {}
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_fibonacci_rec() {
        let source = r#"
            fun fib(n) {
                if (n < 0) {
                    return 0;
                }
                if (n == 1) {
                    return 1;
                }
                var a = fib(n-2);
                var b = fib(n-1);
                return a + b;
            }
            var a = fib(7);
            print a;
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }

    #[test]
    fn vm_fibonacci_iter() {
        let source = r#"
            // Does not really support n below 1.
            fun fib(n) {
                if (n < 1) {
                    return 0;
                }
                var a = 0;
                var b = 1;
                for (var i = 0; i < n-1; i = i + 1) {
                    var c = b;
                    b = a + b;
                    a = c;
                }
                return b;
            }
            var a = fib(20);
            print a;
        "#;
        assert!(VM::new().interpret(source).is_ok());
    }
}
