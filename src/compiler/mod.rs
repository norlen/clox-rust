pub mod chunk;
pub mod compiler;
mod rle;
mod scanner;
mod token;
mod util;

use thiserror::Error;

use scanner::ScannerError;

type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, Clone, Error)]
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

    #[error("Cannot use 'this' outside of a class.")]
    InvalidThis,

    // Used internally in consume to provide error messages to the user.
    #[error("Internal error")]
    InternalError,
}
