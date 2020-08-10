use std::iter::Peekable;
use std::str::CharIndices;

pub struct StrCursor<'a> {
    it: Peekable<CharIndices<'a>>,
    pub index: usize,
}

impl<'a> StrCursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            it: source.char_indices().peekable(),
            index: 0,
        }
    }

    pub fn advance(&mut self) -> Option<char> {
        if let Some((index, ch)) = self.it.next() {
            self.index = index + 1;
            Some(ch)
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        let p = self.it.peek();
        p.map(|v| v.1)
    }

    pub fn peek_next(&mut self) -> Option<char> {
        let it = self.it.clone();
        it.skip(1).next().map(|v| v.1)
    }

    pub fn advance_when<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some((_i, ch)) = self.it.peek() {
            if f(*ch) {
                self.advance();
            } else {
                break;
            }
        }
    }

    pub fn advance_until<F>(&mut self, f: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some((_i, ch)) = self.it.peek() {
            if f(*ch) {
                break;
            }
            self.advance();
        }
    }
}
