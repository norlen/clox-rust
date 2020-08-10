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
    #[error("error scanning source")]
    ScannerError(#[from] ScannerError),
}

pub struct Compiler<'src> {
    parser: Parser<'src>,
    chunk: Chunk,
    source: &'src str,
    scanner: Scanner<'src>,
    mem: &'src mut StringCache,
}

impl<'s, 'src: 's> Compiler<'src> {
    pub fn new(source: &'src str, mem: &'src mut StringCache) -> Self {
        Self {
            parser: Parser::new(),
            chunk: Chunk::new(),
            source,
            scanner: Scanner::new(source),
            mem,
        }
    }

    pub fn compile(mut self) -> Result<Chunk, CompileError> {
        println!("trying to compile: {}", self.source);
        self.advance();
        self.expression();
        self.consume(TokenKind::EOF, "Expect end of expression");
        self.emit_byte(OpCode::Return);

        if self.parser.had_error {
            // return Err()
            todo!();
        } else {
            debug::disassemble_chunk(&self.chunk, "code");
            // todo!();
        }
        Ok(self.chunk)
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current;

        loop {
            match self.scanner.scan_token() {
                Ok(token) => {
                    self.parser.current = Some(token);
                    break;
                }
                Err(e) => {
                    eprintln!("encountered error: {}", e);
                    self.parser
                        .error_at_current(self.parser.current.unwrap().data)
                }
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn consume(&mut self, expected_token: TokenKind, error_msg: &str) {
        if let Some(token) = self.parser.current {
            if token.kind == expected_token {
                self.advance();
                return;
            }
        }
        self.parser.error_at_current(error_msg);
    }

    fn emit_byte(&mut self, op_code: OpCode) {
        self.chunk
            .write(op_code, self.parser.previous.unwrap().line);
    }

    fn emit_constant(&mut self, value: Value) {
        self.chunk
            .add_constant(value, self.parser.previous.unwrap().line)
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenKind::ParenRight, "expect ')' after expression.");
    }

    fn number(&mut self) {
        let value = self.parser.previous.unwrap().data.parse::<f64>().unwrap();
        self.emit_constant(Value::Number(value));
    }

    fn string(&mut self) {
        let src_str = self.parser.previous.as_ref().unwrap().data;
        // Skip " at beginning and end.
        let string = src_str[1..src_str.len() - 1].to_owned();
        let cached_index = self.mem.cache(string);
        self.emit_constant(Value::String(cached_index));
    }

    fn unary(&mut self) {
        let operator_type = self.parser.previous.unwrap().kind;
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenKind::Minus => self.emit_byte(OpCode::Negate),
            TokenKind::Bang => self.emit_byte(OpCode::Not),
            // Unreachable.
            _ => panic!(),
        }
    }

    fn binary(&mut self) {
        let operator_type = self.parser.previous.unwrap().kind;

        // Compile the right operand.
        let rule = self.get_rule(operator_type).unwrap();
        let higher_prec = unsafe { ::std::mem::transmute(rule.precedence as u8 + 1) };
        self.parse_precedence(higher_prec);

        // Emit the operator instruction.
        match operator_type {
            TokenKind::Plus => self.emit_byte(OpCode::Add),
            TokenKind::Minus => self.emit_byte(OpCode::Subtract),
            TokenKind::Star => self.emit_byte(OpCode::Multiply),
            TokenKind::Slash => self.emit_byte(OpCode::Divide),
            TokenKind::BangEqual => {
                self.emit_byte(OpCode::Equal);
                self.emit_byte(OpCode::Not);
            }
            TokenKind::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenKind::Greater => self.emit_byte(OpCode::Greater),
            TokenKind::GreaterEqual => {
                self.emit_byte(OpCode::Less);
                self.emit_byte(OpCode::Not);
            }
            TokenKind::Less => self.emit_byte(OpCode::Less),
            TokenKind::LessEqual => {
                self.emit_byte(OpCode::Greater);
                self.emit_byte(OpCode::Not);
            }
            _ => panic!(),
        }
    }

    fn literal(&mut self) {
        let op_kind = self.parser.previous.unwrap().kind;
        match op_kind {
            TokenKind::Nil => self.emit_byte(OpCode::Nil),
            TokenKind::True => self.emit_byte(OpCode::True),
            TokenKind::False => self.emit_byte(OpCode::False),
            _ => panic!(),
        }
    }

    fn get_rule(&'s self, kind: TokenKind) -> Option<&ParseRule<'s, 'src>> {
        Compiler::RULES_TABLE.get(kind as usize)
    }

    fn parse_precedence(&'s mut self, precedence: Precedence) {
        self.advance();

        let token_kind = self.parser.previous.unwrap().kind;
        let rule = self.get_rule(token_kind).unwrap();
        let prefix_rule = rule.prefix.unwrap();
        prefix_rule(self);

        while precedence
            <= self
                .get_rule(self.parser.current.unwrap().kind)
                .unwrap()
                .precedence
        {
            self.advance();
            let infix_rule = self
                .get_rule(self.parser.previous.unwrap().kind)
                .unwrap()
                .infix
                .unwrap();
            infix_rule(self);
        }
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
        ParseRule { prefix: None                    , infix: None                   , precedence: Precedence::None        }, // Identifier
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

type ParseFunction<'r, 's> = fn(&'r mut Compiler<'s>);

#[derive(Debug)]
struct ParseRule<'r, 's> {
    prefix: Option<ParseFunction<'r, 's>>,
    infix: Option<ParseFunction<'r, 's>>,
    precedence: Precedence,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
enum Precedence {
    None = 0,
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

struct Parser<'a> {
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new() -> Self {
        Self {
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(&self.current.unwrap(), msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(&self.previous.unwrap(), msg);
    }

    fn error_at(&mut self, token: &Token<'_>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        // let where_text = if token.kind == TokenKind::EOF {
        //     "at end"
        // } else {
        //     format!("at {}", )
        // }
        eprintln!("[line {}] Error: {}", token.line, msg);
        self.had_error = true;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_test() {
        let code = "(-1 + 2) * 3 - -4";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(code, &mut cache);
        assert!(compiler.compile().is_ok());
    }

    #[test]
    fn compile_math() {
        let source = "1.5 + 1.3 * 3.5";
        let mut cache = StringCache::new();
        let compiler = Compiler::new(source, &mut cache);
        assert!(compiler.compile().is_ok());
    }
}
