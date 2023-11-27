
use std::fmt::Display;

use crate::{grammar, char_iter::CharIter, src::{Src, Span}, logger::{LoggerRef, Message, Level}};
use unicode_xid::UnicodeXID;

fn closing_paren(ch: char) -> char {
    match ch {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _   => unreachable!(),
    }
}

pub enum TokenKind<'s> {
    Keyword,
    Ident,
    Punct,
    Op,
    Int(i64),
    Float(f64),
    String(String),
    Parentheses(Vec<Token<'s>>),
    Brackets(Vec<Token<'s>>),
    Braces(Vec<Token<'s>>),
    Error(String),
}

pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub raw: &'s str,
    pub span: Span<'s>,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TokenKind::Keyword => write!(f, "keyword {}", self.raw),
            TokenKind::Ident => write!(f, "identifier '{}'", self.raw),
            TokenKind::Punct => write!(f, "'{}'", self.raw),
            TokenKind::Op => write!(f, "operator '{}'", self.raw),
            TokenKind::Int(_) => write!(f, "integer"),
            TokenKind::Float(_) => write!(f, "float"),
            TokenKind::String(_) => write!(f, "string"),
            TokenKind::Parentheses(_) => write!(f, "parenthesized expression"),
            TokenKind::Brackets(_) => write!(f, "bracketed expression"),
            TokenKind::Braces(_) => write!(f, "braced expression"),
            TokenKind::Error(err) => write!(f, "invalid token ({err})"),
        }
    }
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)?;
        if let TokenKind::Parentheses(p) | TokenKind::Brackets(p) | TokenKind::Braces(p) = &self.kind {
            f.debug_list().entries(p.iter()).finish()?;
        }
        write!(f, " ({}..{})", self.span.1.start, self.span.1.end)?;
        Ok(())
    }
}

pub struct Tokenizer<'s, 'g> {
    src: &'s Src,
    iter: CharIter<'s>,
    grammar: &'s grammar::GrammarFile<'g>,
    logger: LoggerRef,
    peek: Option<Token<'s>>,
    pub(crate) last_non_ws: usize,
}

impl std::fmt::Debug for Tokenizer<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Tokenizer")
    }
}

impl<'s, 'g> Tokenizer<'s, 'g> {
    pub fn new(src: &'s Src, grammar: &'s grammar::GrammarFile<'g>, logger: LoggerRef) -> Self {
        let mut ret = Self {
            src, iter: src.iter(),
            grammar, logger, peek: None,
            last_non_ws: 0,
        };
        ret.peek = ret.produce_next();
        ret
    }
    pub fn grammar(&self) -> &'s grammar::GrammarFile<'g> {
        self.grammar
    }
    pub fn logger(&self) -> LoggerRef {
        self.logger.clone()
    }
    pub fn peek(&self) -> Option<&Token<'s>> {
        self.peek.as_ref()
    }
    pub fn eof_span(&self) -> Span<'s> {
        Span(self.src, self.last_non_ws..self.last_non_ws + 1)
    }
    pub fn start_offset(&mut self) -> usize {
        loop {
            // Ignore comments
            if self.iter.peek().is_some_and(|c| c == '/') &&
                self.iter.peek1().is_some_and(|c| c == '/')
            {
                self.iter.next();
                self.iter.next();
                for c in &mut self.iter {
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            // Continue skipping until we encounter a non-whitespace character
            if self.iter.peek().is_some_and(|c| c.is_whitespace()) {
                self.iter.next();
                continue;
            }
            break;
        }
        self.offset()
    }
    pub fn offset(&self) -> usize {
        self.iter.offset() - 1
    }
    fn produce_next(&mut self) -> Option<Token<'s>> {
        macro_rules! nothing {
            ($($tokens: tt)*) => {};
        }

        macro_rules! parse {
            (next $cond: ident $(, $second: ident)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| {
                    self.iter.next();
                    $( self.iter.next(); nothing!($second); )?
                }).is_some()
            };
            (next $cond: ident $(, $second: pat)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| {
                    self.iter.next();
                    $( self.iter.next(); nothing!($second); )?
                }).is_some()
            };
            (next $cond: pat $(, $second: ident)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| {
                    self.iter.next();
                    $( self.iter.next(); nothing!($second); )?
                }).is_some()
            };
            (next $cond: pat $(, $second: pat)? $(,)?) => {
                parse!(peek $cond $(, $second)?).then(|| {
                    self.iter.next();
                    $( self.iter.next(); nothing!($second); )?
                }).is_some()
            };
            (next_while $cond: ident) => { {
                let mut some = false;
                while parse!(peek $cond) {
                    self.iter.next();
                    some = true;
                }
                some
            } };
            (next_while $cond: pat) => { {
                let mut some = false;
                while parse!(peek $cond) {
                    self.iter.next();
                    some = true;
                }
                some
            } };
            (peek $first: ident $(, $second: ident)? $(,)?) => {
                self.iter.peek().is_some_and(|c| c.$first()) $(&& self.iter.peek1().is_some_and(|c| c.$second()))?
            };
            (peek $first: ident $(, $second: pat)? $(,)?) => {
                self.iter.peek().is_some_and(|c| c.$first()) $(&& self.iter.peek1().is_some_and(|c| matches!(c, $second)))?
            };
            (peek $first: pat $(, $second: ident)? $(,)?) => {
                self.iter.peek().is_some_and(|c| matches!(c, $first)) $(&& self.iter.peek1().is_some_and(|c| c.$second()))?
            };
            (peek $first: pat $(, $second: pat)? $(,)?) => {
                self.iter.peek().is_some_and(|c| matches!(c, $first)) $(&& self.iter.peek1().is_some_and(|c| matches!(c, $second)))?
            };
        }

        // Store position for spans, skip whitespace & check for EOF
        self.last_non_ws = self.iter.offset();
        self.start_offset();
        self.iter.peek()?;

        // Store first non-WS position for range of token
        let start = self.offset();

        macro_rules! raw {
            () => {
                &self.iter.src_str()[start..self.offset()]
            };
        }

        macro_rules! make_token {
            ($kind: expr) => { {
                let end = self.offset();
                let raw = &self.iter.src_str()[start..end];
                Some(Token { kind: $kind, raw, span: Span(self.src, start..end) })
            } };
        }

        // Identifier or keyword
        if parse!(next is_xid_start) {
            parse!(next_while is_xid_continue);
            let raw = raw!();
            if self.grammar.keywords.strict.contains(raw) {
                return make_token!(TokenKind::Keyword);
            }
            if self.grammar.keywords.reserved.contains(raw) {
                return make_token!(TokenKind::Error(format!("reserved keyword '{raw}'")));
            }
            return make_token!(TokenKind::Ident);
        }

        // Number
        if parse!(next is_ascii_digit) {
            // Eat all digits
            parse!(next_while is_ascii_digit);

            // If there's a .[0-9]+, then it's a float, 
            // otherwise it should be parsed as a member access like 0.abc
            if parse!(next '.', is_ascii_digit) {
                parse!(next_while is_ascii_digit);
                return match raw!().parse::<f64>() {
                    Ok(num) => make_token!(TokenKind::Float(num)),
                    Err(e) => make_token!(TokenKind::Error(format!("invalid float ({e}"))),
                };
            }
            else {
                return match raw!().parse::<i64>() {
                    Ok(num) => make_token!(TokenKind::Int(num)),
                    Err(e) => make_token!(TokenKind::Error(format!("invalid integer ({e}"))),
                };
            }
        }

        // String
        if parse!(next '"') {
            let mut escaped = String::new();
            while match self.iter.next() {
                Some('"') => {
                    false
                }
                Some(c) => {
                    escaped.push(match c {
                        '\\' => match self.iter.next() {
                            Some('n')  => '\n',
                            Some('t')  => '\t',
                            Some('0')  => '\0',
                            Some('r')  => '\r',
                            Some('\\') => '\\',
                            Some('\"') => '\"',
                            Some('\'') => '\'',
                            Some(c) => {
                                self.logger.lock().unwrap().log(Message::new(
                                    Level::Warning,
                                    format!("Invalid escape sequence '\\{c}'"),
                                    Span(self.src, self.offset() - 1..self.offset())
                                ));
                                c
                            }
                            None => {
                                self.logger.lock().unwrap().log(Message::new(
                                    Level::Warning,
                                    "Expected escape sequence",
                                    Span(self.src, self.offset() - 1..self.offset())
                                ));
                                '\\'
                            }
                        },
                        o => o
                    });
                    true
                }
                None => {
                    return make_token!(TokenKind::Error("unclosed string literal".to_string()));
                }
            } {}
            return make_token!(TokenKind::String(escaped))
        }

        // Punctuation
        if
            // Chained
            parse!(next_while '.') || parse!(next_while ':') ||
            // Single
            parse!(next ',' | ';' | '@') ||
            // Arrows
            parse!(next '-' | '=', '>')
        {
            return make_token!(TokenKind::Punct);
        }

        // Operators
        if parse!(next_while '=' | '+' | '-' | '/' | '%' | '&' | '|' | '^' | '*' | '~' | '!' | '?' | '<' | '>' | '#') {
            return make_token!(TokenKind::Op);
        }

        // Parentheses
        let opening = self.iter.peek().unwrap();
        if parse!(next '(' | '[' | '{') {
            let mut tree = vec![];
            'find_closing: loop {
                // skip whitespace
                self.start_offset();
                match self.iter.peek() {
                    Some(c @ (')' | ']' | '}')) if c == closing_paren(opening) => {
                        self.iter.next();
                        break 'find_closing;
                    },
                    Some(_) => {}
                    None => return make_token!(TokenKind::Error("unclosed parenthesis".to_string())),
                }
                tree.push(self.produce_next().unwrap());
            }
            return make_token!(match opening {
                '(' => TokenKind::Parentheses(tree),
                '[' => TokenKind::Brackets(tree),
                '{' => TokenKind::Braces(tree),
                _ => unreachable!(),
            });
        }

        let c = self.iter.next().unwrap();
        make_token!(TokenKind::Error(format!("invalid character '{c}'")))
    }
}

impl<'s, 'g> Iterator for Tokenizer<'s, 'g> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.produce_next();
        std::mem::replace(&mut self.peek, next)
    }
}
