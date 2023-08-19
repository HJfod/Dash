
use std::path::PathBuf;

pub enum Src {
    Builtin,
    File {
        path: PathBuf,
        data: Vec<char>,
    },
}

impl Src {
    pub fn get(&self, pos: usize) -> Option<char> {
        match self {
            Src::Builtin => None,
            Src::File { path: _, data } => data.get(pos).copied(),
        }
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
        let n = self.src.get(self.pos);
        self.pos += 1;
        n
    }

    pub fn goto(&mut self, pos: usize) {
        self.pos = pos;
    }
}

impl<'s> Parser<'s> {
    pub fn expect_kw(&mut self, kw: &str) -> Result<(), String> {
        
    }
}
