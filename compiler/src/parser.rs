
use std::fmt::Debug;

use crate::src::{Src, Range, Message, Level};
use unicode_xid::UnicodeXID;

pub fn is_op_char(ch: char) -> bool {
    "=+-/%&|^*~@!?<>#".contains(ch)
}

#[derive(PartialEq, Clone)]
pub struct ExprMeta<'s> {
    pub src: &'s Src,
    pub range: Range,
}

impl<'s> ExprMeta<'s> {
    pub fn builtin() -> Self {
        Self { src: Src::builtin(), range: Range::zero() }
    }
}

impl Debug for ExprMeta<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("({:?}:{})", self.src, self.range))
    }
}

pub struct Parser<'s> {
    src: &'s Src,
    pos: usize,
}

impl<'s> Parser<'s> {
    pub fn new(src: &'s Src) -> Self {
        Self {
            src,
            pos: 0,
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn peek(&self) -> Option<char> {
        self.src.get(self.pos)
    }

    pub fn next(&mut self) -> Option<char> {
        // do not advance on None
        self.src.get(self.pos).inspect(|_| self.pos += 1)
    }

    pub fn goto(&mut self, pos: usize) {
        self.pos = pos;
    }
}

impl<'s> Parser<'s> {
    pub fn skip_ws(&mut self) -> usize {
        loop {
            let Some(ch) = self.peek() else { break };
            // ignore comments
            if ch == '/' && self.src.get(self.pos + 1) == Some('/') {
                while let Some(c) = self.next() {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            if !ch.is_whitespace() {
                break;
            }
            self.next();
        }
        self.pos
    }

    pub fn expect_ch(&mut self, ch: char) -> Result<char, Message<'s>> {
        if self.peek() == Some(ch) {
            Ok(self.next().unwrap())
        }
        else {
            Err(self.error(self.pos, format!(
                "Expected '{}', got '{}'",
                ch, self.peek().map(|c| String::from(c)).unwrap_or("EOF".into())
            )))
        }
    }

    pub fn expect_ch_with<F: Fn(char) -> bool>(&mut self, ch: F, name: &str) -> Result<char, Message<'s>> {
        if self.peek().is_some_and(ch) {
            Ok(self.next().unwrap())
        }
        else {
            Err(self.error(self.pos, format!(
                "Expected '{}', got '{}'",
                name, self.peek().map(|c| String::from(c)).unwrap_or("EOF".into())
            )))
        }
    }

    pub fn expect_ch_range(&mut self, ch: std::ops::Range<char>) -> Result<char, Message<'s>> {
        if self.peek().is_some_and(|c| ch.contains(&c)) {
            Ok(self.next().unwrap())
        }
        else {
            Err(self.error(self.pos, format!(
                "Expected '{}'..'{}', got '{}'",
                ch.start, ch.end,
                self.peek().map(|c| String::from(c)).unwrap_or("EOF".into())
            )))
        }
    }

    pub fn next_word(&mut self, word: &str) -> Result<String, Message<'s>> {
        let start = self.skip_ws();
        let Some(ch) = self.next() else {
            return Err(self.error(start, format!("Expected '{word}', got EOF")));
        };
        // a word is either XID_Start XID_Continue*, a single punctuation, 
        // a bunch of dots, or a bunch of operator characters
        if UnicodeXID::is_xid_start(ch) {
            let mut res = String::from(ch);
            while self.peek().is_some_and(|c| UnicodeXID::is_xid_continue(c)) {
                res.push(self.next().unwrap());
            }
            Ok(res)
        }
        // single punctuation
        else if "(){}[],;".contains(ch) {
            Ok(String::from(ch))
        }
        // possibly chained punctuation
        else if ch == '.' || ch == ':' {
            let mut res = String::from(ch);
            while self.peek().is_some_and(|c| c == ch) {
                res.push(self.next().unwrap());
            }
            Ok(res)
        }
        // operator
        else if is_op_char(ch) {
            let mut res = String::from(ch);
            while self.peek().is_some_and(is_op_char) {
                res.push(self.next().unwrap());
            }
            Ok(res)
        }
        else {
            Err(self.error(start, format!("Expected '{word}', got '{}'", ch)))
        }
    }

    pub fn expect_word(&mut self, word: &str) -> Result<String, Message<'s>> {
        let start = self.skip_ws();
        let next = self.next_word(word)?;
        if next == word {
            Ok(word.into())
        }
        else {
            self.goto(start);
            Err(self.error(start, format!("Expected '{word}', got '{next}'")))
        }
    }

    pub fn expect_rule<R: Rule<'s>>(&mut self) -> Result<R, Message<'s>> {
        Rule::expect(self)
    }

    pub fn is_eof(&mut self) -> bool {
        self.skip_ws();
        self.pos >= self.src.len()
    }

    fn last_nws_pos(&self) -> usize {
        let mut i = 1;
        loop {
            let ch = self.src.get(self.pos - i);
            if ch.is_some_and(|c| c.is_whitespace()) {
                i += 1;
                continue;
            }
            break self.pos - i;
        }
    }

    pub fn get_meta(&self, start: usize) -> ExprMeta<'s> {
        ExprMeta {
            src: self.src,
            range: self.src.range(start, self.last_nws_pos() + 1),
        }
    }

    pub fn error<M: Into<String>>(&self, start: usize, message: M) -> Message<'s> {
        Message {
            level: Level::Error,
            info: message.into(),
            notes: Vec::new(),
            src: self.src,
            range: self.src.range(start, self.pos),
        }
    }
}

pub trait Rule<'s>: Sized {
    fn get(parser: &mut Parser<'s>) -> Result<Self, Message<'s>>;
    fn expect(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
        let start = parser.pos();
        let res = Self::get(parser);
        if res.is_err() {
            parser.goto(start);
        }
        res
    }
    fn meta(&self) -> &ExprMeta;
}
