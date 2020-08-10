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
