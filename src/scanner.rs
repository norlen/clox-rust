use crate::token::{Token, TokenKind};
use crate::util::StrCursor;
use radix_trie::Trie;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ScannerError {
    #[error("Unterminated String")]
    UnterminatedString,

    #[error("Invalid Character")]
    InvalidCharacter,
}

pub struct Scanner<'a> {
    source: &'a str,
    cursor: StrCursor<'a>,
    start: usize,
    keywords: Trie<&'static str, TokenKind>,
    line: u64,
}

const KEYWORDS: [(&'static str, TokenKind); 16] = [
    ("and", TokenKind::And),
    ("class", TokenKind::Class),
    ("else", TokenKind::Else),
    ("false", TokenKind::False),
    ("for", TokenKind::For),
    ("fun", TokenKind::Fun),
    ("if", TokenKind::If),
    ("nil", TokenKind::Nil),
    ("or", TokenKind::Or),
    ("print", TokenKind::Print),
    ("return", TokenKind::Return),
    ("super", TokenKind::Super),
    ("this", TokenKind::This),
    ("true", TokenKind::True),
    ("var", TokenKind::Var),
    ("while", TokenKind::While),
];

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut keywords = Trie::new();
        for keyword in KEYWORDS.iter() {
            keywords.insert(keyword.0, keyword.1);
        }

        Self {
            source,
            cursor: StrCursor::new(source),
            start: 0,
            keywords,
            line: 0,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token<'a>, ScannerError> {
        self.skip_whitespace();
        self.start = self.cursor.index;

        if let Some(ch) = self.cursor.advance() {
            let token = match ch {
                '(' => self.create_token(TokenKind::ParenLeft),
                ')' => self.create_token(TokenKind::ParenRight),
                '{' => self.create_token(TokenKind::BraceLeft),
                '}' => self.create_token(TokenKind::BraceRight),
                ';' => self.create_token(TokenKind::Semicolon),
                ',' => self.create_token(TokenKind::Comma),
                '.' => self.create_token(TokenKind::Dot),
                '-' => self.create_token(TokenKind::Minus),
                '+' => self.create_token(TokenKind::Plus),
                '/' => self.create_token(TokenKind::Slash),
                '*' => self.create_token(TokenKind::Star),
                '!' => self.create_token_match('=', TokenKind::BangEqual, TokenKind::Bang),
                '=' => self.create_token_match('=', TokenKind::EqualEqual, TokenKind::Equal),
                '<' => self.create_token_match('=', TokenKind::LessEqual, TokenKind::Less),
                '>' => self.create_token_match('=', TokenKind::GreaterEqual, TokenKind::Greater),
                '"' => self.create_string_token()?,
                ch if ch.is_digit(10) => self.create_digit_token(),
                ch if ch.is_alphabetic() => self.create_identifier_token(),
                _ => return Err(ScannerError::InvalidCharacter),
            };
            // println!("SCANNER\t\t[NEW_TOKEN] {:?}", token);
            Ok(token)
        } else {
            Ok(self.create_token(TokenKind::EOF))
        }
    }

    fn match_token(&mut self, expected: char) -> bool {
        if let Some(ch) = self.cursor.peek() {
            if ch == expected {
                self.cursor.advance();
                return true;
            }
        }
        return false;
    }

    fn create_token(&self, kind: TokenKind) -> Token<'a> {
        let data = &self.source[self.start..self.cursor.index];
        Token::new(kind, data, self.line)
    }

    fn create_token_match(&mut self, matches: char, if_matches: TokenKind, otherwise: TokenKind) -> Token<'a> {
        if self.match_token(matches) {
            self.create_token(if_matches)
        } else {
            self.create_token(otherwise)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.cursor.peek() {
            match ch {
                '\n' => {
                    self.line += 1;
                    self.cursor.advance();
                }
                '/' => {
                    if let Some(ch) = self.cursor.peek_next() {
                        if ch == '/' {
                            // Found comment.
                            while let Some(ch) = self.cursor.advance() {
                                if ch == '\n' {
                                    // Continue if there is even more whitespace on the next line.
                                    continue;
                                }
                            }
                        }
                    }
                    // Otherwise we're done if it isn't a comment or we got None somewhere.
                    return;
                }
                val if val.is_whitespace() => {
                    self.cursor.advance();
                }
                _ => return,
            }
        }
    }

    fn create_identifier_token(&mut self) -> Token<'a> {
        self.cursor
            .advance_when(|ch| ch.is_alphanumeric() || ch == '_');

        let identifier = &self.source[self.start..self.cursor.index];
        if let Some(kind) = self.keywords.get(identifier) {
            self.create_token(*kind)
        } else {
            self.create_token(TokenKind::Identifier)
        }
    }

    fn create_digit_token(&mut self) -> Token<'a> {
        let check_digit = |ch: char| ch.is_digit(10);

        self.cursor.advance_when(check_digit);

        // Check for fractional part and scan that.
        if let Some(ch) = self.cursor.peek() {
            if ch == '.' {
                if let Some(ch) = self.cursor.peek_next() {
                    if check_digit(ch) {
                        self.cursor.advance(); // Consume the dot.
                        self.cursor.advance_when(check_digit)
                    }
                }
            }
        }

        self.create_token(TokenKind::Number)
    }

    fn create_string_token(&mut self) -> Result<Token<'a>, ScannerError> {
        loop {
            if let Some(ch) = self.cursor.peek() {
                if ch == '\n' {
                    self.line += 1;
                }
                self.cursor.advance();
                if ch == '"' {
                    break;
                }
            } else {
                return Err(ScannerError::UnterminatedString);
            }
        }

        Ok(self.create_token(TokenKind::String))
    }
}
