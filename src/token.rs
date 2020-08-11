use std::fmt;

#[derive(Debug, Copy, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub line: u64,
    pub data: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, data: &'a str, line: u64) -> Self {
        Self { kind, data, line }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens.
    ParenLeft,
    ParenRight,
    BraceLeft,
    BraceRight,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    //Error,
    EOF,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            TokenKind::ParenLeft => "ParenLeft",
            TokenKind::ParenRight => "ParenRight",
            TokenKind::BraceLeft => "BraceLeft",
            TokenKind::BraceRight => "BraceRight",
            TokenKind::Comma => "Comma",
            TokenKind::Dot => "Dot",
            TokenKind::Minus => "Minus",
            TokenKind::Plus => "Plus",
            TokenKind::Semicolon => "Semicolon",
            TokenKind::Slash => "Slash",
            TokenKind::Star => "Star",
            TokenKind::Bang => "Bang",
            TokenKind::BangEqual => "BangEqual",
            TokenKind::Equal => "Equal",
            TokenKind::EqualEqual => "EqualEqual",
            TokenKind::Greater => "Greater",
            TokenKind::GreaterEqual => "GreaterEqual",
            TokenKind::Less => "Less",
            TokenKind::LessEqual => "LessEqual",
            TokenKind::Identifier => "Identifier",
            TokenKind::String => "String",
            TokenKind::Number => "Number",
            TokenKind::And => "And",
            TokenKind::Class => "Class",
            TokenKind::Else => "Else",
            TokenKind::False => "False",
            TokenKind::For => "For",
            TokenKind::Fun => "Fun",
            TokenKind::If => "If",
            TokenKind::Nil => "Nil",
            TokenKind::Or => "Or",
            TokenKind::Print => "Print",
            TokenKind::Return => "Return",
            TokenKind::Super => "Super",
            TokenKind::This => "This",
            TokenKind::True => "True",
            TokenKind::Var => "Var",
            TokenKind::While => "While",
            TokenKind::EOF => "EOF",
        };
        write!(f, "{}", s)
    }
}
