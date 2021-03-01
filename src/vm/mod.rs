use crate::compiler::compiler::CompileError;
use std::borrow::Cow;
use thiserror::Error;

mod call_frame;
pub mod instruction;
pub mod value;
pub mod vm;

pub use call_frame::CallFrame;

pub type Result<T> = std::result::Result<T, VMError>;

#[derive(Debug, Error)]
pub enum VMError {
    #[error("Compile error")]
    CompileError(#[from] CompileError),

    #[error("Runtime error")]
    RuntimeError,

    #[error("Runtime error: {}", .0)]
    RuntimeError2(Cow<'static, str>),

    #[error("Type mismatch: {}", .0)]
    TypeError(String),

    #[error("Trying to access empty stack.")]
    EmptyStack,

    #[error("Trying to access empty list of call frames")]
    NoCallFrame,
}
