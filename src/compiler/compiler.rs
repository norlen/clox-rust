use colored::*;
use thiserror::Error;

use super::{
    scanner::{Scanner, ScannerError},
    token::{Token, TokenKind},
};
use crate::debug::{self, LOG_COMPILED_CODE, LOG_COMPILER};
use crate::memory::{Function, Gc, Object, GC};
use crate::vm::{instruction::OpCode, value::Value};

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Number of compiler errors: {}", .0.len())]
    Default(Vec<CompileError>),

    #[error("Error scanning source")]
    ScannerError(#[from] ScannerError),

    #[error("Error parsing number: {}", .0)]
    ParseFloatError(#[from] std::num::ParseFloatError),

    #[error("Could not find token while parsing (should not happen)")]
    TokenNotFound,

    #[error("Parse rule could not be found (should not happen)")]
    ParseRuleNotFound,

    #[error("Error: {}. On line {}", .message, .line)]
    ParseError { message: &'static str, line: u64 },

    #[error("Too many local variables in function.")]
    LocalCount,

    #[error("Cannot jump more than 2^16 bytes.")]
    InvalidJump,

    #[error("Variable {} already declared in this scope", .0)]
    VariableAlreadyDeclared(String),

    #[error("Cannot read local variable in its own initializer.")]
    LocalInitializer,

    // Used internally in consume to provide error messages to the user.
    #[error("Internal error")]
    InternalError,
}

type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug)]
struct Local {
    name: Token,

    // The level of nesting for this local, 0 is the global scope and it moves upwards.
    depth: i64,

    // If any closure has captured this upvalue and it needs to be moved to the heap.
    is_captured: bool,
}

impl Local {
    fn new(name: Token, depth: i64) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }
}

#[derive(Debug)]
struct Upvalue {
    index: u8,
    is_local: bool,
}

impl Upvalue {
    fn new(index: u8, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

#[derive(Debug, PartialEq)]
pub enum FunctionKind {
    Function,
    Script,
}

pub struct Compiler<'src> {
    source: &'src str,
    scanner: Scanner<'src>,
    parser: Parser,

    gc: &'src mut GC,
    errors: Vec<CompileError>,
}

#[derive(Debug)]
pub struct FunctionState {
    pub function: Function,
    function_kind: FunctionKind,
    locals: Vec<Local>,
    scope_depth: i64,
    upvalues: Vec<Upvalue>,
}

impl FunctionState {
    fn script() -> Self {
        Self {
            function: Function::blank(),
            function_kind: FunctionKind::Script,
            // The current function is always the first local, so we need to add one value here.
            locals: vec![Local::new(Token::new_empty(), -1)],
            scope_depth: 0,
            upvalues: Vec::new(),
        }
    }

    fn new(name: Gc<Object>, function_kind: FunctionKind) -> Self {
        Self {
            function: Function::new(name),
            function_kind,
            // The current function is always the first local, so we need to add one value here.
            locals: vec![Local::new(Token::new_empty(), -1)],
            scope_depth: 0,
            upvalues: Vec::new(),
        }
    }

    fn emit_raw(&mut self, byte: u8, line: u64) -> Result<()> {
        if LOG_COMPILER {
            println!(
                "{}\t[EMIT]\t\t\t[BYTE]: {}",
                "[COMPILER]".blue().bold(),
                byte
            );
        }
        self.function.chunk.write_byte(byte, line);
        Ok(())
    }

    fn emit_byte(&mut self, op_code: OpCode, line: u64) -> Result<()> {
        if LOG_COMPILER {
            println!("{}\t[EMIT]\t\t\t{}", "[COMPILER]".blue().bold(), op_code);
        }
        self.function.chunk.write(op_code, line);
        Ok(())
    }

    fn emit_bytes(&mut self, op_code: OpCode, index: u8, line: u64) -> Result<()> {
        if LOG_COMPILER {
            println!(
                "{}\t[EMIT]\t\t\t{} -> {}",
                "[COMPILER]".blue().bold(),
                op_code,
                index
            );
        }
        self.function.chunk.write_index(op_code, index, line);
        Ok(())
    }

    fn emit_return(&mut self, line: u64) -> Result<()> {
        self.emit_byte(OpCode::Nil, line)?;
        self.emit_byte(OpCode::Return, line)
    }

    fn emit_jump(&mut self, op_code: OpCode, line: u64) -> Result<usize> {
        if LOG_COMPILER {
            println!("{}\t[EMIT JMP]\t\t{}", "[COMPILER]".blue().bold(), op_code);
        }
        self.function.chunk.write(op_code, line);
        self.function.chunk.write_byte(0xff, line);
        self.function.chunk.write_byte(0xff, line);
        Ok(self.function.chunk.code.len() - 2)
    }

    /// Emits the loop instruction to jump backwards to `loop_start`. `loop_start` cannot contain
    /// a value higher than `std::u16::MAX` as jumps further than that are not supported yet.
    /// The functions emits `OpCode::Loop` instruction followed by first 16 bits in `loop_start`.
    fn emit_loop(&mut self, loop_start: usize, line: u64) -> Result<()> {
        self.emit_byte(OpCode::Loop, line)?;

        // We have to skip over the next to byte as well which contains the jump location.
        let offset = self.function.chunk.code.len() - loop_start + 2;
        if offset > std::u16::MAX as usize {
            Err(CompileError::InvalidJump)
        } else {
            // let line = self.parser.previous()?.line;
            self.function
                .chunk
                .write_byte((offset >> 8) as u8 & 0xff, line);
            self.function.chunk.write_byte((offset & 0xff) as u8, line);
            Ok(())
        }
    }

    fn patch_jump(&mut self, offset: usize) -> Result<()> {
        let jump_from = self.function.chunk.code.len() as i64;

        // Adjust by -2 to account the the size of the jump bytes.
        let jump = jump_from - offset as i64 - 2;
        if jump > std::u16::MAX as i64 {
            Err(CompileError::InvalidJump)
        } else {
            self.function.chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
            self.function.chunk.code[offset + 1] = (jump & 0xff) as u8;
            Ok(())
        }
    }

    fn resolve_local(&self, token: &Token) -> Result<Option<u8>> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if token.data == local.name.data {
                if local.depth == -1 {
                    // If we want to enable syntax along the lines of
                    //     var a = 0;
                    //     { var a = a; }
                    // We have to make sure this error does not get returned
                    // if we don't have to. But the local will get declared
                    // before this point anyway.
                    return Err(CompileError::LocalInitializer);
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> Result<u8> {
        if self.upvalues.len() == std::u8::MAX as usize {
            panic!("Too many closure variables in function");
        }

        // Check if this upvalue already exists.
        for (i, upvalue) in self.upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return Ok(i as u8);
            }
        }
        self.upvalues.push(Upvalue::new(index, is_local));
        self.function.num_upvalues = self.upvalues.len();
        Ok((self.upvalues.len() - 1) as u8)
    }
}

impl<'s, 'src: 's> Compiler<'src> {
    pub fn new(source: &'src str, gc: &'src mut GC) -> Self {
        Self {
            gc,
            parser: Parser::new(),
            source,
            scanner: Scanner::new(source),
            errors: Vec::new(),
        }
    }

    pub fn compile(mut self) -> Result<Function> {
        if LOG_COMPILED_CODE {
            println!("COMPILING SOURCE: {}", self.source);
        }
        self.advance();

        // Create the main script.
        let function_state = FunctionState::script();
        self.gc.functions.push(function_state);

        while !self.match_token(TokenKind::EOF)? {
            self.decl()?;
        }

        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_return(self.parser.line())?;

        if !self.errors.is_empty() {
            println!("Display all errors encountered:");
            for error in self.errors.iter() {
                println!("\t{}", error);
            }
            Err(CompileError::Default(self.errors))
        } else {
            if LOG_COMPILED_CODE {
                let name = self.gc.functions.last().unwrap().function.function_name();
                debug::disassemble_chunk(&self.gc.functions.last().unwrap().function.chunk, name);
            }
            let fun = self.gc.functions.last().unwrap().function.clone();
            Ok(fun)
        }
    }

    /// Scan for the next token, ignores any errors while scanning.
    /// But they are still added to the errors vector.
    fn advance(&mut self) {
        std::mem::swap(&mut self.parser.previous, &mut self.parser.current);

        loop {
            match self.scanner.scan_token() {
                Ok(token) => {
                    self.parser.current = Some(token);
                    if LOG_COMPILER {
                        println!(
                            "{}\t[ADVANCE]\t\t{:?}",
                            "[COMPILER]".blue(),
                            self.parser.current().unwrap()
                        );
                    }
                    return;
                }
                Err(err) => {
                    eprintln!("Encountered error while scanning: {}", err);
                    self.errors.push(err.into());
                }
            }
        }
    }

    fn match_token(&mut self, kind: TokenKind) -> Result<bool> {
        let current_token = self.parser.current()?;
        if current_token.kind == kind {
            self.advance();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn error_msg(&self, message: &'static str) -> impl FnOnce(CompileError) -> CompileError {
        let line = if let Some(token) = &self.parser.previous {
            token.line
        } else {
            0
        };

        move |error: CompileError| -> CompileError {
            match error {
                CompileError::InternalError => CompileError::ParseError { message, line },
                _ => error,
            }
        }
    }

    fn decl(&mut self) -> Result<()> {
        if let Err(err) = self.declaration() {
            eprintln!("{}\t[ERROR]   {}", "[COMPILER]".red(), err);
            self.errors.push(err);

            // Exit if synchronize encounters any errors.
            self.synchronize()?;
        }
        Ok(())
    }

    fn consume(&mut self, expected_token: TokenKind, error_message: &'static str) -> Result<()> {
        let token = self.parser.current()?;
        if token.kind == expected_token {
            self.advance();
            Ok(())
        } else {
            Err(CompileError::InternalError).map_err(self.error_msg(error_message))
        }
    }

    fn synchronize(&mut self) -> Result<()> {
        // Try to skip tokens until something that looks like a statement boundary is found.
        loop {
            let previous_kind = self.parser.previous()?.kind;
            if previous_kind == TokenKind::Semicolon {
                return Ok(());
            }

            let current_kind = self.parser.current()?.kind;
            match current_kind {
                TokenKind::EOF
                | TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return Ok(()),
                _ => {}
            }

            self.advance();
        }
    }

    fn parse_variable(&mut self, error_msg: &'static str) -> Result<u8> {
        self.consume(TokenKind::Identifier, error_msg)?;

        self.declare_variable()?;
        if self.gc.functions.last().unwrap().scope_depth > 0 {
            return Ok(0);
        }

        Ok(self.identifier_constant(self.parser.previous()?.data.clone()))
    }

    fn declare_variable(&mut self) -> Result<()> {
        // Global variables are implictly declared.
        if self.gc.functions.last().unwrap().scope_depth == 0 {
            return Ok(());
        }
        let name = self.parser.previous()?;
        for local in self.gc.functions.last().unwrap().locals.iter().rev() {
            if local.depth != -1 && local.depth < self.gc.functions.last().unwrap().scope_depth {
                break;
            }
            if name.data == local.name.data {
                return Err(CompileError::VariableAlreadyDeclared(name.data.to_owned()));
            }
        }

        let name = name.clone();
        self.add_local(name)?;
        Ok(())
    }

    fn add_local(&mut self, name: Token) -> Result<()> {
        if self.gc.functions.last().unwrap().locals.len() > std::u8::MAX as usize {
            Err(CompileError::LocalCount)
        } else {
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .locals
                .push(Local::new(name, -1));
            Ok(())
        }
    }

    fn add_constant(&mut self, constant: Value) -> u8 {
        self.gc.stack.push(constant.clone());
        let ret = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .function
            .chunk
            .add_constant(constant);
        self.gc.stack.pop();
        ret
    }

    fn identifier_constant(&mut self, name: String) -> u8 {
        // let cached_string = self.cache.cache(name.to_owned());
        let cached_string = self.gc.track_string(name);
        self.add_constant(cached_string.into())
    }

    fn mark_local_initialized(&mut self) -> Result<()> {
        if self.gc.functions.last().unwrap().scope_depth == 0 {
            return Ok(());
        }
        let new_value = self.gc.functions.last().unwrap().scope_depth;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .locals
            .last_mut()
            .unwrap()
            .depth = new_value;
        Ok(())
    }

    fn define_variable(&mut self, index: u8) -> Result<()> {
        if self.gc.functions.last().unwrap().scope_depth > 0 {
            self.mark_local_initialized()?;
            return Ok(());
        }
        self.gc.functions.last_mut().unwrap().emit_bytes(
            OpCode::DefineGlobal,
            index,
            self.parser.line(),
        )
    }

    fn resolve_local(&self, token: &Token) -> Result<Option<u8>> {
        self.gc.functions.last().unwrap().resolve_local(token)
    }

    fn resolve_upvalue(&mut self, state_index: usize, token: &Token) -> Result<Option<u8>> {
        if state_index == 0 {
            return Ok(None);
        }
        let prev_index = state_index - 1;

        // We want to skip over checking for locals in the current function state.
        if let Some(prev_state) = self.gc.functions.get_mut(prev_index) {

            // See if the previous state has a local variable we want to capture.
            if let Some(local_idx) = prev_state.resolve_local(token)? {
                prev_state.locals[local_idx as usize].is_captured = true;

                // Get the upvalue index.
                let upvalue_idx = self.gc.functions.get_mut(state_index).unwrap().add_upvalue(local_idx, true)?;
                if LOG_COMPILER {
                    println!("[UPVALUE] is_local: {} index: {}", true, upvalue_idx);
                }
                return Ok(Some(upvalue_idx));
            } else {
                // If we couldn't find a local variable, we search recursively.
                if let Some(upvalue_idx) = self.resolve_upvalue(state_index - 1, token)? {
                    let upvalue_idx = self.gc.functions.get_mut(state_index).unwrap().add_upvalue(upvalue_idx, false)?;
                    if LOG_COMPILER {
                        println!("[UPVALUE] is_local: {} index: {}", false, upvalue_idx);
                    }
                    return Ok(Some(upvalue_idx));
                }

            }
        }

        Ok(None)
    }

    fn named_variable(&mut self, token: Token, can_assign: bool) -> Result<()> {
        let (arg, set_op, get_op) = if let Some(arg) = self.resolve_local(&token)? {
            (arg, OpCode::SetLocal, OpCode::GetLocal)
        } else if let Some(arg) = self.resolve_upvalue(self.gc.functions.len() - 1, &token)? {
            (arg, OpCode::SetUpvalue, OpCode::GetUpvalue)
        } else {
            let arg = self.identifier_constant(token.data.clone());
            (arg, OpCode::SetGlobal, OpCode::GetGlobal)
        };

        if LOG_COMPILER {
            println!(
                "{}\t[NAMED VARIABLE]\t{:?} | CAN_ASSIGN: {}",
                "[COMPILER]".blue(),
                token,
                can_assign
            );
        }

        if self.match_token(TokenKind::Equal)? && can_assign {
            self.expression()?;
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_bytes(set_op, arg, self.parser.line())?;
        } else {
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_bytes(get_op, arg, self.parser.line())?;
        }
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        let token = self.parser.previous()?.clone();
        self.named_variable(token, can_assign)?;
        Ok(())
    }

    fn function(&mut self, kind: FunctionKind) -> Result<()> {
        // Set up and compile the a function.
        let last_state = {
            // Create the new state.
            let state = {
                let name = self.parser.previous().unwrap().data.clone();
                let name = self.gc.track_string(name);
                FunctionState::new(name, kind)
            };

            // We always operate on the latest state in the compiler.
            self.gc.functions.push(state);

            // Start compiling the new function.
            self.scope_enter();

            // Compile the parameter list.
            self.consume(TokenKind::ParenLeft, "Expect '(' after function name")?;
            if !self.parser.check_current(TokenKind::ParenRight)? {
                loop {
                    self.gc.functions.last_mut().unwrap().function.arity += 1;
                    if self.gc.functions.last().unwrap().function.arity > 255 {
                        todo!("Cannot have more than 255 parameters.")
                    }

                    let parameter_constant = self.parse_variable("Expect parameter name")?;
                    self.define_variable(parameter_constant)?;

                    if !self.match_token(TokenKind::Comma)? {
                        break;
                    }
                }
            }
            self.consume(TokenKind::ParenRight, "Expect ')' after parameters")?;

            // Compile function body.
            self.consume(TokenKind::BraceLeft, "Expect '{' before function body")?;
            self.block()?;

            // We skip leaving the scope, as those pops shouldn't be needed.
            // We can get and unwrap the last function state as we just created it above.
            let mut state = self.gc.functions.pop().unwrap();

            // Add implicit return if there wasn't a return in the function body.
            if let Some(op) = state.function.chunk.code.last() {
                if *op != OpCode::Return as u8 {
                    state.function.chunk.code.push(OpCode::Return as u8);
                }
            }

            if LOG_COMPILED_CODE {
                debug::disassemble_chunk(&state.function.chunk, state.function.function_name());
            }

            state
        };

        // Add this closure as a constant to the outer one.
        let index = {
            // We ask the GC to track this new function.
            let new_function = self.gc.track_function(last_state.function.clone());
            
            // Not sure if it's because I structured it with an array, but we have to keep track of
            // the compiled functions. We arent't recursing into the constants when marking, so if we
            // have a function as a constant it wont mark all those roots. Temp fix.
            self.gc.compiled_fns.push(new_function.clone());

            self.add_constant(new_function.into())
        };

        // Add upvalues we might have created.
        let state = self.gc.functions.last_mut().unwrap();
        let line = self.parser.line();

        state.emit_bytes(OpCode::Closure, index, line)?;

        println!("{}\tchecking for upvalues:", "[UPVALUE]".red());
        println!("CURRENT FUNC: {}", state.function.function_name());
        println!("{}\t{:?}", "[UPVALUE]".red(), state.upvalues);
        println!("PREVIOUS UPVALUE COUNT: {}", last_state.function.num_upvalues);
        println!("LAST UPVALUES: {:?}", last_state.upvalues);
        for upvalue in last_state.upvalues.iter() {
            let is_local = if upvalue.is_local { 1 } else { 0 };
            state.emit_raw(is_local, line)?;
            state.emit_raw(upvalue.index, line)?;
            println!("{}\tJust emitted: {} {}", "[UPVALUE]".red(), is_local, upvalue.index);
        }

        Ok(())
    }

    fn declaration(&mut self) -> Result<()> {
        if self.match_token(TokenKind::Class)? {
            self.class_declaration()?;
        } else if self.match_token(TokenKind::Fun)? {
            self.fun_declaration()?;
        } else if self.match_token(TokenKind::Var)? {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }
        Ok(())
    }

    fn class_declaration(&mut self) -> Result<()> {
        self.consume(TokenKind::Identifier, "Expect class name")?;
        let name_constant = self.identifier_constant(self.parser.previous()?.data.clone());
        self.declare_variable()?;

        self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_bytes(OpCode::Class, name_constant, self.parser.line())?;
        self.define_variable(name_constant)?;

        self.consume(TokenKind::BraceLeft, "Expect '{' before class body")?;
        self.consume(TokenKind::BraceRight, "Expect '}' after class body")
    }

    fn fun_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expect function name")?;
        self.mark_local_initialized()?;
        self.function(FunctionKind::Function)?;
        self.define_variable(global)
    }

    fn var_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expect variable name")?;

        if self.match_token(TokenKind::Equal)? {
            self.expression()?;
        } else {
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Nil, self.parser.line())?;
        }
        self.consume(
            TokenKind::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        self.define_variable(global)
    }

    fn statement(&mut self) -> Result<()> {
        if self.match_token(TokenKind::Print)? {
            self.print_statement()?;
        } else if self.match_token(TokenKind::If)? {
            self.if_statement()?;
        } else if self.match_token(TokenKind::Return)? {
            self.return_statement()?;
        } else if self.match_token(TokenKind::While)? {
            self.while_statement()?;
        } else if self.match_token(TokenKind::For)? {
            self.for_statement()?;
        } else if self.match_token(TokenKind::BraceLeft)? {
            self.scope_enter();
            self.block()?;
            self.scope_leave()?;
        } else {
            self.expression_statement()?;
        }
        Ok(())
    }

    fn return_statement(&mut self) -> Result<()> {
        if self.gc.functions.last().unwrap().function_kind == FunctionKind::Script {
            panic!("Cannot return from top-level code");
        }

        if self.match_token(TokenKind::Semicolon)? {
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_return(self.parser.line())
        } else {
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after return value")?;
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Return, self.parser.line())
        }
    }

    fn for_statement(&mut self) -> Result<()> {
        self.scope_enter();
        self.consume(TokenKind::ParenLeft, "Expect '(' after 'for'")?;

        // Initializer clause.
        if self.match_token(TokenKind::Semicolon)? {
        } else if self.match_token(TokenKind::Var)? {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        // Condition clause.
        let mut loop_start = self.gc.functions.last().unwrap().function.chunk.code.len();
        let exit_jump = if self.match_token(TokenKind::Semicolon)? {
            None
        } else {
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after loop condition")?;

            // Jump out of the loop if the condition is false.
            let exit_jump = self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_jump(OpCode::JumpIfFalse, self.parser.line())?;
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Pop, self.parser.line())?;
            Some(exit_jump)
        };

        // Increment clause.
        // This is a bit weird, since we want this to run after the for loop body
        // we jump to the increment location (loop_start), we set it to the start
        // of the increment. And since this code is before the loop body we have an
        // unconditional jump that goes straight to the body, and after the expression
        // we jump to the actual start of the loop, i.e. the condition clause.
        if !self.match_token(TokenKind::ParenRight)? {
            let body_jump = self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_jump(OpCode::Jump, self.parser.line())?;
            let increment_start = self.gc.functions.last().unwrap().function.chunk.code.len();

            self.expression()?;
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Pop, self.parser.line())?;
            self.consume(TokenKind::ParenRight, "Expect ')' after for clauses")?;

            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_loop(loop_start, self.parser.line())?;
            loop_start = increment_start;
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .patch_jump(body_jump)?;
        }

        self.statement()?;

        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_loop(loop_start, self.parser.line())?;
        if let Some(exit_jump) = exit_jump {
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .patch_jump(exit_jump)?;
            self.gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Pop, self.parser.line())?;
        }
        self.scope_leave()
    }

    fn while_statement(&mut self) -> Result<()> {
        // Get the location we want to jump to on each loop iteration.
        let loop_start = self.gc.functions.last().unwrap().function.chunk.code.len();

        self.consume(TokenKind::ParenLeft, "Expect '(' after 'while'")?;
        self.expression()?;
        self.consume(TokenKind::ParenRight, "Expect ')' after 'while'")?;

        let exit_jump = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .emit_jump(OpCode::JumpIfFalse, self.parser.line())?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())?;

        self.statement()?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_loop(loop_start, self.parser.line())?;

        self.gc
            .functions
            .last_mut()
            .unwrap()
            .patch_jump(exit_jump)?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())
    }

    fn if_statement(&mut self) -> Result<()> {
        self.consume(TokenKind::ParenLeft, "Expect ')' after 'if'")?;
        self.expression()?;
        self.consume(TokenKind::ParenRight, "Expect ')' after condition")?;

        let then_jump = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .emit_jump(OpCode::JumpIfFalse, self.parser.line())?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())?; // Pop condition if condition is false.
        self.statement()?;
        let else_jump = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .emit_jump(OpCode::Jump, self.parser.line())?;

        self.gc
            .functions
            .last_mut()
            .unwrap()
            .patch_jump(then_jump)?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())?; // Pop condition if condition is true.

        if self.match_token(TokenKind::Else)? {
            self.statement()?;
        }
        self.gc.functions.last_mut().unwrap().patch_jump(else_jump)
    }

    fn scope_enter(&mut self) {
        self.gc.functions.last_mut().unwrap().scope_depth += 1;
    }

    fn scope_leave(&mut self) -> Result<()> {
        println!("Leaving scope");
        let mut fun = self.gc.functions.last_mut().unwrap();
        fun.scope_depth -= 1;
        while let Some(local) = fun.locals.last() {
            println!("\tLOCAL: {:?}", local);
            if local.depth <= fun.scope_depth {
                break;
            }
            if local.is_captured {
                fun.emit_byte(OpCode::CloseUpvalue, self.parser.line())?;
            } else {
                fun.emit_byte(OpCode::Pop, self.parser.line())?;
            }
            fun.locals.pop();
        }
        Ok(())
    }

    fn block(&mut self) -> Result<()> {
        loop {
            let check_brace = self.parser.check_current(TokenKind::BraceRight)?;
            let check_eof = self.parser.check_current(TokenKind::EOF)?;
            if check_brace || check_eof {
                break;
            }
            self.decl()?;
        }
        self.consume(TokenKind::BraceRight, "Expect '}' after a block")
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after expression")?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())?;
        Ok(())
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after value")?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Print, self.parser.line())?;
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::ParenRight, "Expect ')' after expression")?;
        Ok(())
    }

    fn number(&mut self, _can_assign: bool) -> Result<()> {
        let value = self.parser.previous()?.data.parse::<f64>()?;
        let index = self.add_constant(Value::Number(value));
        self.gc.functions.last_mut().unwrap().emit_bytes(
            OpCode::Constant,
            index,
            self.parser.line(),
        )
    }

    fn string(&mut self, _can_assign: bool) -> Result<()> {
        let src_str = self.parser.previous()?.data.clone();
        // Skip " at beginning and end.
        let string = src_str[1..src_str.len() - 1].to_owned();
        let string = self.gc.track_string(string);
        let index = self.add_constant(string.into());
        self.gc.functions.last_mut().unwrap().emit_bytes(
            OpCode::Constant,
            index,
            self.parser.line(),
        )
    }

    fn unary(&mut self, _can_assign: bool) -> Result<()> {
        let operator_type = self.parser.previous()?.kind;
        self.parse_precedence(Precedence::Unary)?;

        match operator_type {
            TokenKind::Minus => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Negate, self.parser.line()),
            TokenKind::Bang => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Not, self.parser.line()),
            // Unreachable.
            _ => panic!(),
        }
    }

    fn and(&mut self) -> Result<()> {
        let end_jump = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .emit_jump(OpCode::JumpIfFalse, self.parser.line())?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())?;
        self.parse_precedence(Precedence::And)?;
        self.gc.functions.last_mut().unwrap().patch_jump(end_jump)
    }

    fn or(&mut self) -> Result<()> {
        let else_jump = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .emit_jump(OpCode::JumpIfFalse, self.parser.line())?;
        let end_jump = self
            .gc
            .functions
            .last_mut()
            .unwrap()
            .emit_jump(OpCode::Jump, self.parser.line())?;

        self.gc
            .functions
            .last_mut()
            .unwrap()
            .patch_jump(else_jump)?;
        self.gc
            .functions
            .last_mut()
            .unwrap()
            .emit_byte(OpCode::Pop, self.parser.line())?;

        self.parse_precedence(Precedence::Or)?;
        self.gc.functions.last_mut().unwrap().patch_jump(end_jump)
    }

    fn call(&mut self) -> Result<()> {
        let arg_count = self.argument_list()?;
        self.gc.functions.last_mut().unwrap().emit_bytes(
            OpCode::Call,
            arg_count,
            self.parser.line(),
        )
    }

    fn argument_list(&mut self) -> Result<u8> {
        let mut arg_count = 0;
        if !self.parser.check_current(TokenKind::ParenRight)? {
            loop {
                self.expression()?;
                arg_count += 1;
                if arg_count > std::u8::MAX as i32 {
                    todo!();
                } else if !self.match_token(TokenKind::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenKind::ParenRight, "Expect ')' after arguments")?;
        Ok(arg_count as u8)
    }

    fn binary(&mut self) -> Result<()> {
        let operator_type = self.parser.previous()?.kind;

        // Compile the right operand.
        let rule = self
            .get_rule(operator_type)
            .ok_or(CompileError::ParseRuleNotFound)?;
        let higher_precedence = rule.precedence.higher();
        if LOG_COMPILER {
            println!(
                "{}\t[BINARY]\t\t{:?} | PRECEDENCE: {:?} | PRECEDENCE_HIGHER: {:?} | OPERATOR: {:?}",
                "[COMPILER]".blue(), rule, rule.precedence, higher_precedence, operator_type
            );
        }
        self.parse_precedence(higher_precedence)?;

        // Emit the operator instruction.
        match operator_type {
            TokenKind::Plus => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Add, self.parser.line())?,
            TokenKind::Minus => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Subtract, self.parser.line())?,
            TokenKind::Star => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Multiply, self.parser.line())?,
            TokenKind::Slash => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Divide, self.parser.line())?,
            TokenKind::BangEqual => {
                self.gc
                    .functions
                    .last_mut()
                    .unwrap()
                    .emit_byte(OpCode::Equal, self.parser.line())?;
                self.gc
                    .functions
                    .last_mut()
                    .unwrap()
                    .emit_byte(OpCode::Not, self.parser.line())?;
            }
            TokenKind::EqualEqual => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Equal, self.parser.line())?,
            TokenKind::Greater => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Greater, self.parser.line())?,
            TokenKind::GreaterEqual => {
                self.gc
                    .functions
                    .last_mut()
                    .unwrap()
                    .emit_byte(OpCode::Less, self.parser.line())?;
                self.gc
                    .functions
                    .last_mut()
                    .unwrap()
                    .emit_byte(OpCode::Not, self.parser.line())?;
            }
            TokenKind::Less => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Less, self.parser.line())?,
            TokenKind::LessEqual => {
                self.gc
                    .functions
                    .last_mut()
                    .unwrap()
                    .emit_byte(OpCode::Greater, self.parser.line())?;
                self.gc
                    .functions
                    .last_mut()
                    .unwrap()
                    .emit_byte(OpCode::Not, self.parser.line())?;
            }
            _ => panic!(),
        }
        Ok(())
    }

    fn literal(&mut self, _can_assign: bool) -> Result<()> {
        let op_kind = self.parser.previous()?.kind;
        match op_kind {
            TokenKind::Nil => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::Nil, self.parser.line())?,
            TokenKind::True => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::True, self.parser.line())?,
            TokenKind::False => self
                .gc
                .functions
                .last_mut()
                .unwrap()
                .emit_byte(OpCode::False, self.parser.line())?,
            _ => panic!(),
        }
        Ok(())
    }

    fn get_rule(&'s self, kind: TokenKind) -> Option<&ParseRule<'s, 'src>> {
        Compiler::RULES_TABLE.get(kind as usize)
    }

    fn parse_precedence(&'s mut self, precedence: Precedence) -> Result<()> {
        self.advance();

        let token_kind = self.parser.previous()?.kind;
        let rule = self
            .get_rule(token_kind)
            .ok_or(CompileError::ParseRuleNotFound)?;
        let prefix_rule = rule.prefix.ok_or(CompileError::ParseRuleNotFound)?;

        let can_assign = precedence <= Precedence::Assignment;
        prefix_rule(self, can_assign)?;

        while precedence
            <= self
                .get_rule(self.parser.current()?.kind)
                .ok_or(CompileError::ParseRuleNotFound)?
                .precedence
        {
            self.advance();
            let infix_rule = self
                .get_rule(self.parser.previous()?.kind)
                .ok_or(CompileError::ParseRuleNotFound)?
                .infix
                .ok_or(CompileError::ParseRuleNotFound)?;
            infix_rule(self)?;
        }

        if can_assign && self.match_token(TokenKind::Equal)? {
            panic!("invalid assigment target");
        }

        Ok(())
    }

    #[rustfmt::skip]
    const RULES_TABLE: [ParseRule<'s, 'src>; 39] = [
        ParseRule { prefix: Some(Compiler::grouping), infix: Some(Compiler::call)   , precedence: Precedence::Call        }, // ParenLeft
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // ParenRight
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // BraceLeft
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // BraceRight
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Comma
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Dot
        ParseRule { prefix: Some(Compiler::unary)   , infix: Some(Compiler::binary) , precedence: Precedence::Term        }, // Minus
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Term        }, // Plus
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Semicolon
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Factor      }, // Slash
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Factor      }, // Star
        ParseRule { prefix: Some(Compiler::unary)   , infix: None                   , precedence: Precedence::None        }, // Bang
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Equality    }, // BangEqual
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Equal
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Equality    }, // EqualEqual
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Comparison  }, // Greater
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Comparison  }, // GreaterEqual
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Comparison  }, // Less
        ParseRule { prefix: None                    , infix: Some(Compiler::binary) , precedence: Precedence::Comparison  }, // LessEqual
        ParseRule { prefix: Some(Compiler::variable), infix: None                   , precedence: Precedence::None        }, // Identifier
        ParseRule { prefix: Some(Compiler::string)  , infix: None                   , precedence: Precedence::None        }, // String
        ParseRule { prefix: Some(Compiler::number)  , infix: None                   , precedence: Precedence::None        }, // Number
        ParseRule { prefix: None                    , infix: Some(Compiler::and)    , precedence: Precedence::And         }, // And
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Class
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Else
        ParseRule { prefix: Some(Compiler::literal) , infix: None                   , precedence: Precedence::None        }, // False
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // For
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Fun
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // If
        ParseRule { prefix: Some(Compiler::literal) , infix: None                   , precedence: Precedence::None        }, // Nil
        ParseRule { prefix: None                    , infix: Some(Compiler::or)     , precedence: Precedence::Or          }, // Or
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Print
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Return
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Super
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // This
        ParseRule { prefix: Some(Compiler::literal) , infix: None                   , precedence: Precedence::None        }, // True
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Var
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // While
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // EOF
    ];
}

type PrefixFunction<'r, 's> = fn(&'r mut Compiler<'s>, bool) -> Result<()>;
type InfixFunction<'r, 's> = fn(&'r mut Compiler<'s>) -> Result<()>;

#[derive(Debug)]
struct ParseRule<'r, 's> {
    prefix: Option<PrefixFunction<'r, 's>>,
    infix: Option<InfixFunction<'r, 's>>,
    precedence: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // ==, !=
    Comparison, // <, >, <=, >=
    Term,       // +, -
    Factor,     // *, /
    Unary,      // !, -
    Call,       // ., ()
    Primary,
}

impl Precedence {
    fn higher(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
}

impl Parser {
    fn new() -> Self {
        Self {
            current: None,
            previous: None,
        }
    }

    fn previous(&self) -> Result<&Token> {
        self.previous.as_ref().ok_or(CompileError::TokenNotFound)
    }

    fn current(&self) -> Result<&Token> {
        self.current.as_ref().ok_or(CompileError::TokenNotFound)
    }

    fn check_current(&self, kind: TokenKind) -> Result<bool> {
        if self.current()?.kind == kind {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn line(&self) -> u64 {
        self.previous.as_ref().unwrap().line
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile(source: &'static str) -> Result<Function> {
        let mut gc = GC::new();
        let compiler = Compiler::new(source, &mut gc);
        compiler.compile()
    }

    #[test]
    fn simple_test() {
        let source = "(-1 + 2) * 3 - -4;";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_math() {
        let source = "1.5 + 1.3 * 3.5;";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_print() {
        let source = "print 1;";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_constants() {
        let source = r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;
        "#;
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_weird_assignments() {
        let source = "a * b = c + d;";
        assert!(compile(source).is_err());
    }

    #[test]
    fn compile_weird_expr() {
        let source = "1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12;";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_assign_to_itself() {
        let source = r#"
        var a = 1;
        {
            var a = a;
            {
                var a = a;
            }
        }"#;
        assert!(compile(source).is_err());
    }

    #[test]
    fn compile_if_statement() {
        let source = "if (1) {}";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_and_expr() {
        let source = "print true and false;";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_or_expr() {
        let source = "print true or false;";
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_while() {
        assert!(compile("while (true) {}").is_ok());
    }

    #[test]
    fn compile_for_basic() {
        assert!(compile("for (var i = 0; i < 10; i = i + 1) {}").is_ok());
    }

    #[test]
    fn compile_fun_simple() {
        let source = r#"
        fun hello() {}
        fun hello2(a) {}
        "#;
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_fibonacci_rec() {
        let source = r#"
            fun fib(n) {
                if (n == 0) {
                    return 0;
                }
                return fib(n-2) + fib(n-1);
            }
            var a = fib(20);
            print a;
        "#;
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_fibonacci() {
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
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_closure0() {
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
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_closure1() {
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
        assert!(compile(source).is_ok());
    }

    #[test]
    fn compile_class_declarations() {
        let source = r#"
            class Brioche {}
            print Brioche;
        "#;
        assert!(compile(source).is_ok());
    }
}
