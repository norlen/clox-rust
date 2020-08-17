use colored::*;
use thiserror::Error;

use crate::compiler::CompileError;
use crate::debug::{self, TRACE_EXECUTION_INSTR, TRACE_EXECUTION_STACK};
use crate::gc::GC;
use crate::instruction::OpCode;
use crate::object::Allocated;
use crate::object::Object;
use crate::value::*;

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

#[derive(Clone)]
pub struct CallFrame {
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
        self.function
            .chunk
            .code
            .get(self.ip - 1)
            .copied()
            .ok_or(VMError::RuntimeError)
    }

    fn peek_instruction(&mut self, offset: i64) -> Result<u8> {
        let index = self.function.chunk.code.len() as i64 - offset;
        assert!(self.function.chunk.code.len() < std::i64::MAX as usize);
        assert!(index > 0);
        self.function
            .chunk
            .code
            .get(index as usize)
            .copied()
            .ok_or(VMError::RuntimeError)
    }

    fn next_instruction_as_constant(&mut self) -> Result<&Value> {
        let index = self.next_instruction()?;
        self.function
            .chunk
            .constants
            .get(index as usize)
            .ok_or(VMError::RuntimeError)
    }

    fn next_instruction_as_jump(&mut self) -> Result<usize> {
        let b0 = self.next_instruction()? as usize;
        let b1 = self.next_instruction()? as usize;
        Ok(b0 << 8 | b1)
    }
}

pub struct VM<'gc> {
    gc: &'gc mut GC,
}

impl<'gc> VM<'gc> {
    pub fn new(gc: &'gc mut GC) -> Self {
        let mut vm = Self { gc };

        vm.define_native("clock".to_owned(), native_clock);
        vm
    }

    pub fn interpret_function(&mut self, func: Function) -> Result<()> {
        let tracked_func = self.gc.track_function(func.clone());
        self.gc.stack.push(Value::Object(tracked_func));
        self.gc
            .call_frames
            .push(CallFrame::new(func, self.gc.stack.len() - 1));

        if let Err(err) = self.run() {
            // Stack trace.
            for frame in self.gc.call_frames.iter().rev() {
                let name = frame.function.function_name();
                let instruction = frame.ip - 1;
                let line = frame
                    .function
                    .chunk
                    .lines
                    .get(instruction)
                    .copied()
                    .unwrap_or(0);
                println!("[line {}] in {}", line, name);
            }
            return Err(err);
        }
        Ok(())
    }

    fn define_native(&mut self, name: String, native_fun: NativeFunction) {
        let name_obj = self.gc.track_string(name.clone());
        self.gc.stack.push(Value::Object(name_obj.clone())); // Make it reachable.

        let native_fn = self.gc.track_native(NativeFn::new(name_obj, native_fun));
        self.gc.stack.push(Value::Object(native_fn.clone())); // Make this reachable as well.

        self.gc.globals.insert(name, Value::Object(native_fn));

        // They are in the globals table now and does not need to be reachable from the stack.
        self.gc.stack.pop();
        self.gc.stack.pop();
    }

    fn run(&mut self) -> Result<()> {
        let mut frame = self.gc.call_frames.pop().ok_or(VMError::RuntimeError)?;

        while frame.ip < frame.function.chunk.code.len() {
            let instruction = frame.next_instruction()?;
            let instruction = OpCode::from(instruction);

            if TRACE_EXECUTION_STACK || TRACE_EXECUTION_INSTR {
                let r = debug::disassemble_instruction(&frame.function.chunk, frame.ip - 1);
                if TRACE_EXECUTION_STACK {
                    if self.gc.stack.len() > 0 {
                        let mut stack_str = Vec::new();
                        for val in self.gc.stack.iter() {
                            let ss = match val {
                                Value::Object(object) => format!(" [{}]", object.get().data),
                                _ => format!(" [{}]", val),
                            };
                            stack_str.push(ss);
                        }
                        let stack = stack_str.iter().fold(String::new(), |acc, s| acc + s);
                        println!("\n{}\t\t{}", "[STACK]".yellow(), stack.trim_start());
                    } else {
                        println!("\n{}", "[STACK]".yellow());
                    }
                }
                if TRACE_EXECUTION_INSTR {
                    println!("{}\t{:04}\t{}", "[Instruction]".green(), frame.ip - 1, r.0);
                }
            }

            match instruction {
                OpCode::Return => {
                    self.op_return(&mut frame)?;
                }
                OpCode::Constant => {
                    let constant = frame.next_instruction_as_constant()?;
                    let constant = constant.clone();
                    self.gc.stack.push(constant);
                }
                OpCode::Nil => {
                    self.gc.stack.push(Value::Nil);
                }
                OpCode::True => {
                    self.gc.stack.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.gc.stack.push(Value::Bool(false));
                }
                OpCode::Equal => {
                    let rhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    let result = Value::equals(lhs, rhs).ok_or_else(|| {
                        VMError::TypeError("cannot compare these values".to_owned())
                    })?;
                    self.gc.stack.push(Value::Bool(result));
                }
                OpCode::Greater => {
                    let rhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.gc.stack.push(Value::Bool(lhs > rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Less => {
                    let rhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (lhs, rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.gc.stack.push(Value::Bool(lhs < rhs))
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Not => {
                    let value = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    let value = match value {
                        Value::Bool(value) => Ok(!value),
                        Value::Nil => Ok(true),
                        _ => Err(VMError::TypeError("expected a nil or bool".to_owned())),
                    }?;
                    self.gc.stack.push(Value::Bool(value));
                }
                OpCode::Negate => {
                    let value = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    match value {
                        Value::Number(v) => {
                            self.gc.stack.push(Value::Number(-v));
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Add => {
                    let rhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    let lhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    match (&lhs, &rhs) {
                        (Value::Number(lhs), Value::Number(rhs)) => {
                            self.gc.stack.push(Value::Number(lhs + rhs))
                        }
                        (Value::Object(lhs), Value::Object(rhs)) => {
                            match (&lhs.get().data, &rhs.get().data) {
                                (Object::String(lhs), Object::String(rhs)) => {
                                    let new = lhs.clone() + rhs;
                                    let new = self.gc.track_string(new);
                                    self.gc.stack.push(Value::Object(new));
                                }
                                _ => todo!(),
                            }
                        }
                        _ => {
                            return Err(VMError::TypeError("operand must be a number.".to_owned()))
                        }
                    }
                }
                OpCode::Subtract => {
                    self.op_binary(|lhs, rhs| lhs - rhs)?;
                }
                OpCode::Multiply => {
                    self.op_binary(|lhs, rhs| lhs * rhs)?;
                }
                OpCode::Divide => {
                    self.op_binary(|lhs, rhs| lhs / rhs)?;
                }
                OpCode::Print => {
                    let value = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
                    match &value {
                        Value::Object(object) => match &object.get().data {
                            Object::String(object) => println!("{}", object),
                            _ => panic!("trying to print rnadom objects"),
                        },
                        _ => println!("{}", value),
                    };
                }
                OpCode::Pop => {
                    // We don't care about the value here, used in expression statements.
                    self.gc.stack.pop();
                }
                OpCode::DefineGlobal => {
                    self.op_define_global(&mut frame)?;
                }

                OpCode::GetGlobal => {
                    self.op_get_global(&mut frame).unwrap();
                }
                OpCode::SetGlobal => {
                    self.op_set_global(&mut frame).unwrap();
                }
                OpCode::GetLocal => {
                    let index = frame.next_instruction()? as usize + frame.stack_base;
                    let value = self
                        .gc
                        .stack
                        .get(index)
                        .ok_or(VMError::RuntimeError)?
                        .clone();
                    self.gc.stack.push(value);
                }
                OpCode::SetLocal => {
                    let index = frame.next_instruction()?;
                    let value = self.gc.stack.last().ok_or(VMError::RuntimeError)?.clone();
                    self.gc.stack[index as usize + frame.stack_base] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = frame.next_instruction_as_jump()?;
                    let condition_value = match self.gc.stack.last().ok_or(VMError::RuntimeError)? {
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
                    let fun_index = self.gc.stack.len() - arg_count - 1;
                    self.gc.call_frames.push(frame);

                    let fun = self.gc.stack.get(fun_index).ok_or(VMError::RuntimeError)?;
                    let fun = fun.clone();

                    self.call_value(fun, arg_count)?;
                    frame = self.gc.call_frames.pop().unwrap();
                }
            }
        }
        Ok(())
    }

    fn op_binary(&mut self, op: fn(f64, f64) -> f64) -> Result<()> {
        let rhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
        let lhs = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => {
                self.gc.stack.push(Value::Number(op(lhs, rhs)))
            }
            _ => return Err(VMError::TypeError("operand must be a number.".to_owned())),
        }
        Ok(())
    }

    fn op_return(&mut self, frame: &mut CallFrame) -> Result<()> {
        let result = self.gc.stack.pop().ok_or(VMError::RuntimeError)?;

        if self.gc.call_frames.len() == 0 {
            self.gc.stack.pop();
            return Ok(());
        }

        self.gc.stack.truncate(frame.stack_base);
        *frame = self.gc.call_frames.pop().unwrap();

        self.gc.stack.push(result);
        Ok(())
    }

    fn op_define_global(&mut self, frame: &mut CallFrame) -> Result<()> {
        let next_instruction = frame.next_instruction_as_constant()?;
        let string = self.get_string_object(next_instruction)?;
        let value = self
            .gc
            .stack
            .get(self.gc.stack.len() - 1)
            .ok_or(VMError::RuntimeError)?;
        self.gc.globals.insert(string.clone(), value.clone());
        self.gc.stack.pop();
        Ok(())
    }

    fn op_get_global(&mut self, frame: &mut CallFrame) -> Result<()> {
        let next_instruction = frame.next_instruction_as_constant()?;
        let string = self.get_string_object(next_instruction)?;
        let value = self.gc.globals.get(string).unwrap();
        self.gc.stack.push(value.clone());
        Ok(())
    }

    fn op_set_global(&mut self, frame: &mut CallFrame) -> Result<()> {
        let next_instruction = frame.next_instruction_as_constant()?;
        let string = self.get_string_object(next_instruction)?;
        let value = self.gc.stack.last().unwrap();
        self.gc.globals.insert(string.clone(), value.clone());
        Ok(())
    }

    fn get_object<'a>(&self, value: &'a Value) -> Result<&'a Allocated<Object>> {
        match value {
            Value::Object(object) => Ok(object),
            _ => panic!("expected object"),
        }
    }

    fn get_string_object<'a>(&self, value: &'a Value) -> Result<&'a String> {
        match value {
            Value::Object(object) => match &object.get().data {
                Object::String(string) => return Ok(string),
                _ => panic!("expected string object for global variable"),
            },
            _ => panic!("expected object"),
        }
    }

    fn call_value(&mut self, fun: Value, arg_count: usize) -> Result<()> {
        match &fun {
            Value::Object(object) => match &object.get().data {
                Object::Function(fun) => self.call(&fun, arg_count),
                Object::Native(native_fn) => {
                    let s = self.gc.stack.len() - arg_count - 1;
                    let e = self.gc.stack.len();
                    let native_fn = native_fn.fun;
                    let result = native_fn(arg_count, &self.gc.stack[s..e]);
                    self.gc.stack.truncate(s);
                    self.gc.stack.push(result);
                    Ok(())
                }
                _ => panic!(),
            },
            _ => Err(VMError::RuntimeError),
        }
    }

    fn call(&mut self, fun: &Function, arg_count: usize) -> Result<()> {
        if arg_count != fun.arity as usize {
            panic!("Expected {} arguments but got {}", fun.arity, arg_count);
        }

        let call_frame = CallFrame::new(fun.clone(), self.gc.stack.len() - arg_count - 1);
        self.gc.call_frames.push(call_frame);

        if self.gc.call_frames.len() > 255 {
            // We should have some limit at least.
            panic!("Stack overflow");
        }

        Ok(())
    }
}

fn native_clock(_arg_count: usize, _args: &[Value]) -> Value {
    let time_now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap();
    Value::Number(time_now.as_millis() as f64 / 1000f64)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(source: &str) -> std::result::Result<(), Box<dyn std::error::Error>> {
        use crate::compiler::Compiler;

        let mut gc = GC::new();

        let function = {
            let compiler = Compiler::new(source, &mut gc);
            compiler.compile()?
        };

        let mut vm = VM::new(&mut gc);
        vm.interpret_function(function)?;
        Ok(())
    }

    #[test]
    fn vm_raw_instructions() {
        use crate::chunk::Chunk;
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
        add_constant(&mut chunk, 2.5);
        chunk.write(instruction::OpCode::Return, 0);

        let mut fun = Function::blank();
        fun.chunk = chunk;

        let mut gc = GC::new();
        let mut vm = VM::new(&mut gc);
        assert!(vm.interpret_function(fun).is_ok());
    }

    #[test]
    fn vm_math0() {
        let source = "(-1 + 2) * 3 - -4;";
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_math1() {
        let source = "!(5 - 4 > 3 * 2 == !nil);";
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_string0() {
        let source = "\"st\" + \"ri\" + \"ng\";";
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_string_interning() {
        let source = "\"hello\" + \"hello\" + \"hello\";";
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_print() {
        let source = "print 3 + (4 * 3) * (1 + (2 + 3));";
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_constants() {
        let source = r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_locals_simple() {
        let source = "{ var a = 2; }";
        assert!(run(source).is_ok());
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
        assert!(run(source).is_ok());
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
        assert!(run(source).is_ok());
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
        assert!(run(source).is_ok());
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
        assert!(run(source).is_ok());
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
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_while() {
        let source = r#"
        var a = 0;
        while (a < 10) {
            a = a + 1;
        }
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_for() {
        let source = r#"
        for (var i = 0; i < 10; i = i + 1) {
            print i;
        }
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_fun_simple() {
        let source = r#"
        fun hello() {}
        fun hello2(a) {}
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_fibonacci_rec() {
        let source = r#"
            fun fib(n) {
                if (n < 2) return n;
                return fib(n - 2) + fib(n - 1);
            }
            
            var start = clock();
            print fib(7);
            print clock() - start;
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_fibonacci_iter() {
        let source = r#"
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
            var start = clock();
            print fib(20);
            print clock() - start;
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_gc0() {
        let source = r#"
        var a = 0;
        var b = "hello world";
        {
            var c = b + "!";
            print c;
        }
        b = "goodbye";
        {
            var d = b + " world";
            print d;
        }
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_closure0() {
        let source = r#"
            var x = "global";
            fun outer() {
                var x = "outer";
                fun inner() {
                    print x;
                }
                inner();
            }
            outer();
        "#;
        assert!(run(source).is_ok());
    }

    #[test]
    fn vm_closure1() {
        let source = r#"
            fun makeClosure() {
                var local = "local";
                fun closure() {
                    print local;
                }
                return closure;
            }
            var closure = makeClosure();
            closure();
        "#;
        assert!(run(source).is_ok());
    }
}
