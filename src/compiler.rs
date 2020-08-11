use crate::chunk::Chunk;
use crate::debug;
use crate::instruction::OpCode;
use crate::scanner::{Scanner, ScannerError};
use crate::string_cache::StringCache;
use crate::token::{Token, TokenKind};
use crate::value::Value;
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

pub struct Compiler<'src> {
    parser: Parser<'src>,
    chunk: Chunk,
    source: &'src str,
    scanner: Scanner<'src>,
    mem: &'src mut StringCache,
    locals: Vec<Local<'src>>,
    local_count: i64,
    scope_depth: i64,
    errors: Vec<CompileError>,
}

impl<'s, 'src: 's> Compiler<'src> {
    pub fn new(source: &'src str, mem: &'src mut StringCache) -> Self {
        Self {
            parser: Parser::new(),
            chunk: Chunk::new(),
            source,
            scanner: Scanner::new(source),
            mem,
            locals: Vec::new(),
            local_count: 0,
            scope_depth: 0,
            errors: Vec::new(),
        }
    }

    pub fn compile(mut self) -> Result<Chunk> {
        println!("COMPILING SOURCE: {}", self.source);
        self.advance();

        while !self.match_token(TokenKind::EOF)? {
            self.decl()?;
        }

        self.emit_byte(OpCode::Return)?;

        if !self.errors.is_empty() {
            println!("Display all errors encountered:");
            for error in self.errors.iter() {
                println!("\t{}", error);
            }
            Err(CompileError::Default(self.errors))
        } else {
            debug::disassemble_chunk(&self.chunk, &self.mem, "code");
            Ok(self.chunk)
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

    fn decl(&mut self) -> Result<()> {
        if let Err(err) = self.declaration() {
            eprintln!("COMPILER\t[ERROR]   {}", err);
            self.errors.push(err);

            // Exit if synchronize encounters any errors.
            self.synchronize()?;
        }
        Ok(())
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

    fn consume(&mut self, expected_token: TokenKind) -> Result<()> {
        let token = self.parser.current()?;
        if token.kind == expected_token {
            self.advance();
            Ok(())
        } else {
            Err(CompileError::InternalError)
        }
    }

    fn emit_byte(&mut self, op_code: OpCode) -> Result<()> {
        println!("COMPILER\t[EMIT]\t\t\t{}", op_code);
        let line = self.parser.previous()?.line;
        self.chunk.write(op_code, line);
        Ok(())
    }

    fn emit_bytes(&mut self, op_code: OpCode, index: u8) -> Result<()> {
        println!("COMPILER\t[EMIT]\t\t\t{} -> {}", op_code, index);
        let line = self.parser.previous()?.line;
        self.chunk.write_index(op_code, index, line);
        Ok(())
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

    fn parse_variable(&mut self) -> Result<u8> {
        self.consume(TokenKind::Identifier)?;

        self.declare_variable()?;
        if self.scope_depth > 0 {
            return Ok(0);
        }

        Ok(self.identifier_constant(self.parser.previous()?.data))
    }

    fn declare_variable(&mut self) -> Result<()> {
        // Global variables are implictly declared.
        if self.scope_depth == 0 {
            return Ok(());
        }
        let name = self.parser.previous()?;
        for local in self.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.scope_depth {
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
        if self.locals.len() > std::u8::MAX as usize {
            Err(CompileError::LocalCount)
        } else {
            self.locals.push(Local::new(name, -1));
            Ok(())
        }
    }

    fn identifier_constant(&mut self, name: &str) -> u8 {
        let cached_string = self.mem.cache(name.to_owned());
        self.chunk.add_constant(Value::String(cached_string))
    }

    fn mark_local_initialized(&mut self) -> Result<()> {
        self.locals.last_mut().unwrap().depth = self.scope_depth;
        Ok(())
    }

    fn define_variable(&mut self, index: u8) -> Result<()> {
        if self.scope_depth > 0 {
            self.mark_local_initialized()?;
            return Ok(());
        }
        self.emit_bytes(OpCode::DefineGlobal, index)
    }

    fn resolve_local(&mut self, token: Token<'_>) -> Result<Option<u8>> {
        for (i, local) in self.locals.iter().enumerate().rev() {
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

    fn declaration(&mut self) -> Result<()> {
        if self.match_token(TokenKind::Var)? {
            self.var_declaration()?;
        } else {
            self.statement()?;
        }
        Ok(())
    }

    fn var_declaration(&mut self) -> Result<()> {
        let global = self
            .parse_variable()
            .map_err(self.error_msg("Expect variable name"))?;

        if self.match_token(TokenKind::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::Nil)?;
        }
        self.consume(TokenKind::Semicolon)
            .map_err(self.error_msg("Expect ';' after variable declaration"))?;

        self.define_variable(global)?;
        Ok(())
    }

    fn statement(&mut self) -> Result<()> {
        if self.match_token(TokenKind::Print)? {
            self.print_statement()?;
        } else if self.match_token(TokenKind::BraceLeft)? {
            self.scope_enter();
            self.block()?;
            self.scope_leave()?;
        } else {
            self.expression_statement()?;
        }
        Ok(())
    }

    fn scope_enter(&mut self) {
        self.scope_depth += 1;
    }

    fn scope_leave(&mut self) -> Result<()> {
        self.scope_depth -= 1;
        while let Some(local) = self.locals.last() {
            if local.depth <= self.scope_depth {
                break;
            }
            self.locals.pop();
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
        self.consume(TokenKind::BraceRight).map_err(self.error_msg("Expect '}' after a block"))
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::Semicolon)
            .map_err(self.error_msg("Expect ';' after expression"))?;
        self.emit_byte(OpCode::Pop)?;
        Ok(())
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::Semicolon)
            .map_err(self.error_msg("Expect ';' after value"))?;
        self.emit_byte(OpCode::Print)?;
        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<()> {
        self.expression()?;
        self.consume(TokenKind::ParenRight)
            .map_err(self.error_msg("Expect ')' after expression"))?;
        Ok(())
    }

    fn number(&mut self, _can_assign: bool) -> Result<()> {
        let value = self.parser.previous()?.data.parse::<f64>()?;
        let index = self.chunk.add_constant(Value::Number(value));
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
        let cached_index = self.mem.cache(string);
        let index = self.chunk.add_constant(Value::String(cached_index));
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
        ParseRule { prefix: Some(Compiler::grouping), infix: None                   , precedence: Precedence::None        }, // ParenLeft
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
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // And
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Class
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Else
        ParseRule { prefix: Some(Compiler::literal) , infix: None                   , precedence: Precedence::None        }, // False
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // For
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Fun
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // If
        ParseRule { prefix: Some(Compiler::literal) , infix: None                   , precedence: Precedence::None        }, // Nil
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Or
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

    #[test]
    fn simple_test() {
        let code = "(-1 + 2) * 3 - -4;";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(code, &mut cache);
        assert!(compiler.compile().is_ok());
    }

    #[test]
    fn compile_math() {
        let source = "1.5 + 1.3 * 3.5;";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_ok());
    }

    #[test]
    fn compile_print() {
        let source = "print 1;";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_ok());
    }

    #[test]
    fn compile_constants() {
        let source = r#"
        var beverage = "cafe au lait";
        var breakfast = "beignets with " + beverage;
        print breakfast;
        "#;
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_ok());
    }

    #[test]
    fn compile_weird_assignments() {
        let source = "a * b = c + d;";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_err());
    }

    #[test]
    fn compile_weird_expr() {
        let source = "1 * 2 * 3 * 4 * 5 * 6 * 7 * 8 * 9 * 10 * 11 * 12;";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_ok());
    }

    #[test]
    fn compile_shadow_global_same_name() {
        let source = "var a = 1; { var a = a; }";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_ok());
    }
}
