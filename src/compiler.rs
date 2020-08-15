use crate::debug;
use crate::instruction::OpCode;
use crate::scanner::{Scanner, ScannerError};
use crate::string_cache::StringCache;
use crate::token::{Token, TokenKind};
use crate::value::{Value, Function};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("Number of compiler errors: {}", .0.len())]
    Default(Vec<CompileError>),

    #[error("Error scanning source")]
    ScannerError(#[from] ScannerError),

    #[error("Error parsing number: {}", .0)]
    ParseFloatError(#[from] std::num::ParseFloatError),

    #[error("Unexpected token: {}", .0)]
    UnexpectedToken(TokenKind),

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
struct Local<'a> {
    name: Token<'a>,

    // The level of nesting for this local, 0 is the global scope and it moves upwards.
    depth: i64,
}

impl<'a> Local<'a> {
    fn new(name: Token<'a>, depth: i64) -> Self {
        Self { name, depth }
    }
}

#[derive(PartialEq)]
enum FunctionKind {
    Function,
    Script,
}

pub struct Compiler<'src> {
    parser: Parser<'src>,
    source: &'src str,
    scanner: Scanner<'src>,
    cache: &'src mut StringCache,
    errors: Vec<CompileError>,
    fun_state: FunctionState<'src>,
}

struct FunctionState<'src> {
    function: Function,
    function_kind: FunctionKind,
    locals: Vec<Local<'src>>,
    scope_depth: i64,
}

impl<'src> FunctionState<'src> {
    fn new(function_kind: FunctionKind) -> Self {
        Self {
            function: Function::blank(),
            function_kind,
            // The current function is always the first local, so we need to add one value here.
            locals: vec![Local::new(Token::new_empty(), -1)],
            scope_depth: 0,
        }
    }
}

impl<'s, 'src: 's> Compiler<'src> {
    pub fn new(source: &'src str, cache: &'src mut StringCache) -> Self {
        Self {
            parser: Parser::new(),
            source,
            scanner: Scanner::new(source),
            cache,
            errors: Vec::new(),
            fun_state: FunctionState::new(FunctionKind::Script),
        }
    }

    pub fn compile(mut self) -> Result<Function> {
        println!("COMPILING SOURCE: {}", self.source);
        self.advance();

        while !self.match_token(TokenKind::EOF)? {
            self.decl()?;
        }

        self.emit_return()?;

        if !self.errors.is_empty() {
            println!("Display all errors encountered:");
            for error in self.errors.iter() {
                println!("\t{}", error);
            }
            Err(CompileError::Default(self.errors))
        } else {
            let name = if self.fun_state.function.name == "" {
                "<script>"
            } else {
                self.fun_state.function.name.as_str()
            };
            debug::disassemble_chunk(&self.fun_state.function.chunk, &self.cache, name);
            Ok(self.fun_state.function)
        }
    }
    
    /// Scan for the next token, ignores any errors while scanning.
    /// But they are still added to the errors vector.
    fn advance(&mut self) {
        self.parser.previous = self.parser.current;
        
        loop {
            match self.scanner.scan_token() {
                Ok(token) => {
                    self.parser.current = Some(token);
                    println!("COMPILER\t[ADVANCE]\t\t{:?}", self.parser.current().unwrap());
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
        let line = if let Some(token) = self.parser.previous {
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
            eprintln!("COMPILER\t[ERROR]   {}", err);
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

    fn emit_byte(&mut self, op_code: OpCode) -> Result<()> {
        println!("COMPILER\t[EMIT]\t\t\t{}", op_code);
        let line = self.parser.previous()?.line;
        self.fun_state.function.chunk.write(op_code, line);
        Ok(())
    }

    fn emit_bytes(&mut self, op_code: OpCode, index: u8) -> Result<()> {
        println!("COMPILER\t[EMIT]\t\t\t{} -> {}", op_code, index);
        let line = self.parser.previous()?.line;
        self.fun_state.function.chunk.write_index(op_code, index, line);
        Ok(())
    }

    fn emit_return(&mut self) -> Result<()> {
        self.emit_byte(OpCode::Nil)?;
        self.emit_byte(OpCode::Return)
    }

    fn emit_jump(&mut self, op_code: OpCode) -> Result<usize> {
        println!("COMPILER\t[EMIT JMP]\t\t{}", op_code);
        let line = self.parser.previous()?.line;
        self.fun_state.function.chunk.write(op_code, line);
        self.fun_state.function.chunk.write_byte(0xff, line);
        self.fun_state.function.chunk.write_byte(0xff, line);
        Ok(self.fun_state.function.chunk.code.len() - 2)
    }

    /// Emits the loop instruction to jump backwards to `loop_start`. `loop_start` cannot contain
    /// a value higher than `std::u16::MAX` as jumps further than that are not supported yet.
    /// The functions emits `OpCode::Loop` instruction followed by first 16 bits in `loop_start`.
    fn emit_loop(&mut self, loop_start: usize) -> Result<()> {
        self.emit_byte(OpCode::Loop)?;

        // We have to skip over the next to byte as well which contains the jump location.
        let offset = self.fun_state.function.chunk.code.len() - loop_start + 2;
        if offset > std::u16::MAX as usize {
            Err(CompileError::InvalidJump)
        } else {
            let line = self.parser.previous()?.line;
            self.fun_state.function.chunk.write_byte((offset >> 8) as u8 & 0xff, line);
            self.fun_state.function.chunk.write_byte((offset & 0xff) as u8, line);
            Ok(())
        }
    }

    fn patch_jump(&mut self, offset: usize) -> Result<()> {
        let jump_from = self.fun_state.function.chunk.code.len() as i64;

        // Adjust by -2 to account the the size of the jump bytes.
        let jump = jump_from - offset as i64 - 2;
        if jump > std::u16::MAX as i64 {
            Err(CompileError::InvalidJump)
        } else {
            self.fun_state.function.chunk.code[offset] = ((jump >> 8) & 0xff) as u8;
            self.fun_state.function.chunk.code[offset + 1] = (jump & 0xff) as u8;
            Ok(())
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
        if self.fun_state.scope_depth > 0 {
            return Ok(0);
        }

        Ok(self.identifier_constant(self.parser.previous()?.data))
    }

    fn declare_variable(&mut self) -> Result<()> {
        // Global variables are implictly declared.
        if self.fun_state.scope_depth == 0 {
            return Ok(());
        }
        let name = self.parser.previous()?;
        for local in self.fun_state.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.fun_state.scope_depth {
                break;
            }
            if name.data == local.name.data {
                return Err(CompileError::VariableAlreadyDeclared(name.data.to_owned()));
            }
        }

        self.add_local(name)?;
        Ok(())
    }

    fn add_local(&mut self, name: Token<'src>) -> Result<()> {
        if self.fun_state.locals.len() > std::u8::MAX as usize {
            Err(CompileError::LocalCount)
        } else {
            self.fun_state.locals.push(Local::new(name, -1));
            Ok(())
        }
    }

    fn identifier_constant(&mut self, name: &str) -> u8 {
        let cached_string = self.cache.cache(name.to_owned());
        self.fun_state.function.chunk.add_constant(Value::String(cached_string))
    }

    fn mark_local_initialized(&mut self) -> Result<()> {
        if self.fun_state.scope_depth == 0 {
            return Ok(())
        }
        self.fun_state.locals.last_mut().unwrap().depth = self.fun_state.scope_depth;
        Ok(())
    }

    fn define_variable(&mut self, index: u8) -> Result<()> {
        if self.fun_state.scope_depth > 0 {
            self.mark_local_initialized()?;
            return Ok(());
        }
        self.emit_bytes(OpCode::DefineGlobal, index)
    }

    fn resolve_local(&mut self, token: Token<'_>) -> Result<Option<u8>> {
        for (i, local) in self.fun_state.locals.iter().enumerate().rev() {
            println!("LOCAL: {:?}", local);
            if token.data == local.name.data  {
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

    fn named_variable(&mut self, token: Token<'_>, can_assign: bool) -> Result<()> {
        let (arg, set_op, get_op) = if let Some(arg) = self.resolve_local(token)? {
            (arg, OpCode::SetLocal, OpCode::GetLocal)
        } else {
            let arg = self.identifier_constant(token.data);
            (arg, OpCode::SetGlobal, OpCode::GetGlobal)
        };

        println!(
            "COMPILER\t[NAMED VARIABLE]\t{:?} | CAN_ASSIGN: {}",
            token, can_assign
        );

        if self.match_token(TokenKind::Equal)? && can_assign {
            self.expression()?;
            self.emit_bytes(set_op, arg)?;
        } else {
            self.emit_bytes(get_op, arg)?;
        }
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> Result<()> {
        self.named_variable(self.parser.previous()?, can_assign)?;
        Ok(())
    }

    fn function(&mut self, kind: FunctionKind) -> Result<()> {
        let mut state = FunctionState::new(kind);
        state.function.name = self.parser.previous.unwrap().data.to_owned();
        std::mem::swap(&mut self.fun_state, &mut state);

        self.scope_enter();

        // Compile the parameter list.
        self.consume(TokenKind::ParenLeft, "Expect '(' after function name")?;
        if !self.parser.check_current(TokenKind::ParenRight)? {
            loop {
                self.fun_state.function.arity += 1;
                if self.fun_state.function.arity > 255 {
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
        
        // Swap back and add new function as a constant.
        std::mem::swap(&mut self.fun_state, &mut state);
        let index = self.fun_state.function.chunk.add_constant(Value::Function(state.function));
        self.emit_bytes(OpCode::Constant, index)?;
        Ok(())
    }

    fn declaration(&mut self) -> Result<()> {
        if self.match_token(TokenKind::Fun)? {
            self.fun_declaration()?;
        } else if self.match_token(TokenKind::Var)? {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }
        Ok(())
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
            self.emit_byte(OpCode::Nil)?;
        }
        self.consume(TokenKind::Semicolon, "Expect ';' after variable declaration")?;

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
        if self.fun_state.function_kind == FunctionKind::Script {
            panic!("Cannot return from top-level code");
        }

        if self.match_token(TokenKind::Semicolon)? {
            self.emit_return()
        } else {
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after return value")?;
            self.emit_byte(OpCode::Return)
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
        let mut loop_start = self.fun_state.function.chunk.code.len();
        let exit_jump = if self.match_token(TokenKind::Semicolon)? {
            None
        } else {
            self.expression()?;
            self.consume(TokenKind::Semicolon, "Expect ';' after loop condition")?;

            // Jump out of the loop if the condition is false.
            let exit_jump = self.emit_jump(OpCode::JumpIfFalse)?;
            self.emit_byte(OpCode::Pop)?;
            Some(exit_jump)
        };
        
        // Increment clause.
        // This is a bit weird, since we want this to run after the for loop body
        // we jump to the increment location (loop_start), we set it to the start
        // of the increment. And since this code is before the loop body we have an
        // unconditional jump that goes straight to the body, and after the expression
        // we jump to the actual start of the loop, i.e. the condition clause.
        if !self.match_token(TokenKind::ParenRight)? {
            let body_jump = self.emit_jump(OpCode::Jump)?;
            let increment_start = self.fun_state.function.chunk.code.len();

            self.expression()?;
            self.emit_byte(OpCode::Pop)?;
            self.consume(TokenKind::ParenRight, "Expect ')' after for clauses")?;
            
            self.emit_loop(loop_start)?;
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        }

        self.statement()?;

        self.emit_loop(loop_start)?;
        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
            self.emit_byte(OpCode::Pop)?;
        }
        self.scope_leave()
    }

    fn while_statement(&mut self) -> Result<()> {
        // Get the location we want to jump to on each loop iteration.
        let loop_start = self.fun_state.function.chunk.code.len();

        self.consume(TokenKind::ParenLeft, "Expect '(' after 'while'")?;
        self.expression()?;
        self.consume(TokenKind::ParenRight, "Expect ')' after 'while'")?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse)?;
        self.emit_byte(OpCode::Pop)?;

        self.statement()?;
        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jump)?;
        self.emit_byte(OpCode::Pop)
    }

    fn if_statement(&mut self) -> Result<()> {
        self.consume(TokenKind::ParenLeft, "Expect ')' after 'if'")?;
        self.expression()?;
        self.consume(TokenKind::ParenRight, "Expect ')' after condition")?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse)?;
        self.emit_byte(OpCode::Pop)?; // Pop condition if condition is false.
        self.statement()?;
        let else_jump = self.emit_jump(OpCode::Jump)?;

        self.patch_jump(then_jump)?;
        self.emit_byte(OpCode::Pop)?; // Pop condition if condition is true.
    
        if self.match_token(TokenKind::Else)? {
            self.statement()?;
        }
        self.patch_jump(else_jump)
    }

    fn scope_enter(&mut self) {
        self.fun_state.scope_depth += 1;
    }

    fn scope_leave(&mut self) -> Result<()> {
        self.fun_state.scope_depth -= 1;
        while let Some(local) = self.fun_state.locals.last() {
            if local.depth <= self.fun_state.scope_depth {
                break;
            }
            self.fun_state.locals.pop();
            self.emit_byte(OpCode::Pop)?;
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
        self.emit_byte(OpCode::Pop)?;
        Ok(())
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::Semicolon, "Expect ';' after value")?;
        self.emit_byte(OpCode::Print)?;
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
        let index = self.fun_state.function.chunk.add_constant(Value::Number(value));
        self.emit_bytes(OpCode::Constant, index)
    }

    fn string(&mut self, _can_assign: bool) -> Result<()> {
        let src_str = self
            .parser
            .previous
            .as_ref()
            .ok_or(CompileError::TokenNotFound)?
            .data;
        // Skip " at beginning and end.
        let string = src_str[1..src_str.len() - 1].to_owned();
        let cached_index = self.cache.cache(string);
        let index = self.fun_state.function.chunk.add_constant(Value::String(cached_index));
        self.emit_bytes(OpCode::Constant, index)
    }

    fn unary(&mut self, _can_assign: bool) -> Result<()> {
        let operator_type = self.parser.previous()?.kind;
        self.parse_precedence(Precedence::Unary)?;

        match operator_type {
            TokenKind::Minus => self.emit_byte(OpCode::Negate),
            TokenKind::Bang => self.emit_byte(OpCode::Not),
            // Unreachable.
            _ => panic!(),
        }
    }

    fn and(&mut self) -> Result<()> {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse)?;
        self.emit_byte(OpCode::Pop)?;
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump)
    }

    fn or(&mut self) -> Result<()> {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse)?;
        let end_jump = self.emit_jump(OpCode::Jump)?;

        self.patch_jump(else_jump)?;
        self.emit_byte(OpCode::Pop)?;

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump)
    }

    fn call(&mut self) -> Result<()> {
        let arg_count = self.argument_list()?;
        self.emit_bytes(OpCode::Call, arg_count)
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
        println!(
            "COMPILER\t[BINARY]\t\t{:?} | PRECEDENCE: {:?} | PRECEDENCE_HIGHER: {:?} | OPERATOR: {:?}",
            rule, rule.precedence, higher_precedence, operator_type
        );
        self.parse_precedence(higher_precedence)?;

        // Emit the operator instruction.
        match operator_type {
            TokenKind::Plus => self.emit_byte(OpCode::Add)?,
            TokenKind::Minus => self.emit_byte(OpCode::Subtract)?,
            TokenKind::Star => self.emit_byte(OpCode::Multiply)?,
            TokenKind::Slash => self.emit_byte(OpCode::Divide)?,
            TokenKind::BangEqual => {
                self.emit_byte(OpCode::Equal)?;
                self.emit_byte(OpCode::Not)?;
            }
            TokenKind::EqualEqual => self.emit_byte(OpCode::Equal)?,
            TokenKind::Greater => self.emit_byte(OpCode::Greater)?,
            TokenKind::GreaterEqual => {
                self.emit_byte(OpCode::Less)?;
                self.emit_byte(OpCode::Not)?;
            }
            TokenKind::Less => self.emit_byte(OpCode::Less)?,
            TokenKind::LessEqual => {
                self.emit_byte(OpCode::Greater)?;
                self.emit_byte(OpCode::Not)?;
            }
            _ => panic!(),
        }
        Ok(())
    }

    fn literal(&mut self, _can_assign: bool) -> Result<()> {
        let op_kind = self.parser.previous()?.kind;
        match op_kind {
            TokenKind::Nil => self.emit_byte(OpCode::Nil)?,
            TokenKind::True => self.emit_byte(OpCode::True)?,
            TokenKind::False => self.emit_byte(OpCode::False)?,
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

    fn lower(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::None,
            Precedence::Assignment => Precedence::None,
            Precedence::Or => Precedence::Assignment,
            Precedence::And => Precedence::Or,
            Precedence::Equality => Precedence::And,
            Precedence::Comparison => Precedence::Equality,
            Precedence::Term => Precedence::Comparison,
            Precedence::Factor => Precedence::Term,
            Precedence::Unary => Precedence::Factor,
            Precedence::Call => Precedence::Unary,
            Precedence::Primary => Precedence::Call,
        }
    }
}

struct Parser<'a> {
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    fn new() -> Self {
        Self {
            current: None,
            previous: None,
        }
    }

    fn previous(&self) -> Result<Token<'a>> {
        self.previous.ok_or(CompileError::TokenNotFound)
    }

    fn current(&self) -> Result<Token<'a>> {
        self.current.ok_or(CompileError::TokenNotFound)
    }

    fn check_current(&self, kind: TokenKind) -> Result<bool> {
        if self.current()?.kind == kind {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile(source: &'static str) -> Result<Function> {
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
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
}
