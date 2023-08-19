
use crate::src::{Src, Range, Message, Level};
use unicode_xid::UnicodeXID;

#[derive(Debug, PartialEq, Clone)]
pub struct ExprMeta<'s> {
    pub src: &'s Src,
    pub range: Range,
}

impl<'s> ExprMeta<'s> {
    pub fn builtin() -> Self {
        Self { src: Src::builtin(), range: Range::zero() }
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
    fn skip_ws(&mut self) -> usize {
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

    fn next_word(&mut self, word: &str) -> Result<String, Message<'s>> {
        const OP_CHARS: &str = "=+-/%&|^*~@!?<>#";
        let start = self.skip_ws();
        let Some(ch) = self.next() else {
            return Err(self.error(start, format!("Expected '{word}', got EOF")));
        };
        // a word is either XID_Start XID_Continue*, a single punctuation, 
        // a bunch of dots, or a bunch of operator characters
        if UnicodeXID::is_xid_start(ch) {
            let res = String::from(ch);
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
            let res = String::from(ch);
            while self.peek().is_some_and(|c| c == ch) {
                res.push(self.next().unwrap());
            }
            Ok(res)
        }
        // operator
        else if OP_CHARS.contains(ch) {
            let res = String::from(ch);
            while self.peek().is_some_and(|c| OP_CHARS.contains(c)) {
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
        let start = parser.skip_ws();
        let res = Self::get(parser);
        if res.is_err() {
            parser.goto(start);
        }
        res
    }
}

#[allow(non_camel_case_types)]
struct XID_Start<'s> {
    meta: ExprMeta<'s>,
}

impl<'s> Rule<'s> for XID_Start<'s> {
    fn get(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
        if parser.peek().is_some_and(|c| UnicodeXID::is_xid_start(c)) {
            Ok(parser.next().unwrap())
        }
        else {
            Err(parser.error(parser.pos(), format!(
                "Expected identifier or keyword, got '{}'",
                parser.peek().map(|c| String::from(c)).unwrap_or("EOF".into())
            )))
        }
    }
}

#[allow(non_camel_case_types)]
struct XID_Continue<'s> {
    meta: ExprMeta<'s>,
}

impl<'s> Rule<'s> for XID_Continue<'s> {
    fn get(parser: &mut Parser<'s>) -> Result<Self, Message<'s>> {
        if parser.peek().is_some_and(|c| UnicodeXID::is_xid_continue(c)) {
            Ok(parser.next().unwrap())
        }
        else {
            Err(parser.error(parser.pos(), format!(
                "Expected identifier or keyword, got '{}'",
                parser.peek().map(|c| String::from(c)).unwrap_or("EOF".into())
            )))
        }
    }
}
