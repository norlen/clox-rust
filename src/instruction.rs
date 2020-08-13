use std::fmt;

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    // ConstantLong,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Negate,
    Not,
    Return,
    Add,
    Subtract,
    Multiply,
    Divide,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

impl From<u8> for OpCode {
    fn from(op_code: u8) -> OpCode {
        unsafe { ::std::mem::transmute(op_code) }
    }
}

impl From<&u8> for OpCode {
    fn from(op_code: &u8) -> OpCode {
        OpCode::from(*op_code)
    }
}

impl OpCode {
    pub fn name(&self) -> &str {
        match self {
            OpCode::Constant => "Constant",
            // OpCode::ConstantLong => "ConstantLong",
            OpCode::Nil => "Nil",
            OpCode::True => "True",
            OpCode::False => "False",
            OpCode::Equal => "Equal",
            OpCode::Greater => "Greater",
            OpCode::Less => "Less",
            OpCode::Return => "Return",
            OpCode::Negate => "Negate",
            OpCode::Not => "Not",
            OpCode::Add => "Add",
            OpCode::Subtract => "Subtract",
            OpCode::Multiply => "Multiply",
            OpCode::Divide => "Divide",
            OpCode::Print => "Print",
            OpCode::Pop => "Pop",
            OpCode::DefineGlobal => "DefineGlobal",
            OpCode::GetGlobal => "GetGlobal",
            OpCode::SetGlobal => "SetGlobal",
            OpCode::GetLocal => "GetLocal",
            OpCode::SetLocal => "SetLocal",
            OpCode::JumpIfFalse => "JumpIfFalse",
            OpCode::Jump => "Jump",
            OpCode::Loop => "Loop",
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
