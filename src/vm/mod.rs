use crate::compiler::compiler::CompileError;
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

    #[error("Type mismatch: {}", .0)]
    TypeError(String),
}
