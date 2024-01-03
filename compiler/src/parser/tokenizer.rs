
use std::fmt::Display;
use std::ops::Range;

use crate::shared::char_iter::CharIter;
use crate::shared::src::{Src, Span};
use crate::shared::logger::{LoggerRef, Message, Level};
use unicode_xid::UnicodeXID;

pub(crate) const STRICT_KEYWORDS: &[&str] = &[
    // Literals
    "void", "true", "false", "none",
    // Constants & special variables
    "this", "super",
    // Declarations
    "var", "let", "fun", "struct", "enum", "using",
    "macro", "extends", "module", "type",
    // Prepositions
    "in", "is", "as", "where", "from",
    // Loops & conditionals
    "if", "else", "for", "while",
    // Control flow
    "try", "return", "break", "continue",
    // Visibility
    "extern", "public", "private",
    // Types
    "typeof", "const",
    // Other
    "codegen", "compiler_intrinsic"
];
pub(crate) const RESERVED_KEYWORDS: &[&str] = &[
    // Declarations
    "trait", "class", "interface",
    // Control flow
    "unwrap", "yield", "match", "switch",
    // Visibility
    "export", "import",
    // Reactivity
    "depends", "required",
    // Macros
    "reflect", "codegen",
    // Other
    "mut", "mutable", "new", "null"
];

pub const MAX_PEEK_COUNT: usize = 2;

fn closing_paren(ch: char) -> char {
    match ch {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _   => unreachable!(),
    }
}

trait IsTokenChar {
    fn is_op_char(&self) -> bool;
    fn is_punct_char(&self) -> bool;
}

impl IsTokenChar for char {
    fn is_op_char(&self) -> bool {
        matches!(self, '=' | '+' | '-' | '/' | '%' | '&' | '|' | '^' | '*' | '~' | '!' | '?' | '<' | '>' | '#')
    }
    fn is_punct_char(&self) -> bool {
        matches!(self, ',' | ';' | '.' | ':' | '@')
    }
}

pub enum TokenKind<'s> {
    Keyword,
    Ident,
    Punct,
    Int(i64),
    Float(f64),
    String(String),
    Parentheses(TokenTree<'s>),
    Brackets(TokenTree<'s>),
    Braces(TokenTree<'s>),
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
        // if let TokenKind::Parentheses(p) | TokenKind::Brackets(p) | TokenKind::Braces(p) = &self.kind {
            // f.debug_list().entries(p.items).finish()?;
        // }
        write!(f, " ({}..{})", self.span.1.start, self.span.1.end)?;
        Ok(())
    }
}

pub struct Tokenizer<'s> {
    src: &'s Src,
    iter: CharIter<'s>,
    logger: LoggerRef,
}

impl std::fmt::Debug for Tokenizer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Tokenizer")
    }
}

impl<'s> Tokenizer<'s> {
    pub fn new(src: &'s Src, logger: LoggerRef) -> Self {
        Self { src, iter: src.iter(), logger, }
    }
    fn skip_ws(&mut self) {
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
    }
    fn offset(&self) -> usize {
        self.iter.offset() - 1
    }
}

impl<'s> Iterator for Tokenizer<'s> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
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

        // Skip whitespace & check for EOF
        self.skip_ws();
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
            if STRICT_KEYWORDS.contains(&raw) {
                return make_token!(TokenKind::Keyword);
            }
            if RESERVED_KEYWORDS.contains(&raw) {
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
            parse!(next '-' | '=', '>') ||
            // Operator
            parse!(next_while is_op_char)
        {
            return make_token!(TokenKind::Punct);
        }

        // Parentheses
        let opening = self.iter.peek().unwrap();
        if parse!(next '(' | '[' | '{') {
            let mut items = vec![];
            'find_closing: loop {
                // skip whitespace
                self.skip_ws();
                match self.iter.peek() {
                    Some(c @ (')' | ']' | '}')) if c == closing_paren(opening) => {
                        self.iter.next();
                        break 'find_closing;
                    },
                    Some(_) => {}
                    None => return make_token!(TokenKind::Error("unclosed parenthesis".to_string())),
                }
                items.push(self.next().unwrap());
            }
            let tree = TokenTree {
                src: self.src,
                items: items.into_iter(),
                start_offset: start,
                eof: self.offset() - 1..self.offset(),
                logger: self.logger.clone(),
            };
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

pub struct TokenTree<'s> {
    src: &'s Src,
    items: std::vec::IntoIter<Token<'s>>,
    start_offset: usize,
    eof: Range<usize>,
    logger: LoggerRef,
}

impl<'s> Iterator for TokenTree<'s> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        self.items.next()
    }
}

enum TokenIterSrc<'s> {
    Tokenizer(Tokenizer<'s>),
    Tree(TokenTree<'s>),
}

impl<'s> Iterator for TokenIterSrc<'s> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Tokenizer(t) => t.next(),
            Self::Tree(t) => t.next(),
        }
    }
}

pub struct TokenIterator<'s> {
    src: &'s Src,
    iter: TokenIterSrc<'s>,
    peek: [Option<Token<'s>>; MAX_PEEK_COUNT],
    start_of_last_token: usize,
    last_was_braced: bool,
    eof: Option<Range<usize>>,
    logger: LoggerRef,
}

impl<'s> TokenIterator<'s> {
    fn new(
        src: &'s Src,
        start_offset: usize,
        eof: Option<Range<usize>>,
        logger: LoggerRef,
        mut iter: TokenIterSrc<'s>,
    ) -> Self {
        let peek = core::array::from_fn(|_| iter.next());
        Self {
            src, logger, iter, peek,
            start_of_last_token: start_offset, eof,
            last_was_braced: false,
        }
    }
    pub fn peek(&self, n: usize) -> Option<&Token<'s>> {
        self.peek[n].as_ref()
    }
    pub(crate) fn last_was_braced(&self) -> bool {
        self.last_was_braced
    }
    fn eof_span(&self) -> Span<'s> {
        if let Some(r) = self.eof.clone() {
            Span(self.src, r)
        }
        else {
            Span(self.src, self.start_of_last_token - 1..self.start_of_last_token)
        }
    }
    fn eof_name(&self) -> String {
        self.eof.as_ref()
            .map(|c| self.src.data().chars().skip(c.start).take(c.end - c.start).collect::<String>())
            .unwrap_or(String::from("end-of-file"))
    }
    pub(crate) fn logger(&self) -> LoggerRef {
        self.logger.clone()
    }
    pub fn error<S: Display>(&mut self, msg: S) {
        if let Some(token) = self.next() {
            self.logger.lock().unwrap().log(Message::new(Level::Error, msg, token.span));
        }
        else {
            self.logger.lock().unwrap().log(Message::new(Level::Error, msg, self.eof_span()))
        }
    }
    pub fn expected<S: Display>(&mut self, expected: S) {
        self.error(if let Some(token) = &self.peek(0) {
            format!("Expected {expected}, got {token}")
        }
        else {
            format!("Expected {expected}, got {}", self.eof_name())
        })
    }
    pub fn expected_eof(&mut self) {
        self.expected(self.eof_name())
    }
    /// Constructs an empty TokenTree. Exists for the sake of the #[token] 
    /// attribute being able to construct TokenKinds with subtrees
    pub(crate) fn empty_tree(&self) -> TokenTree<'s> {
        TokenTree {
            src: self.src,
            items: vec![].into_iter(),
            start_offset: 0,
            eof: 0..0,
            logger: self.logger.clone()
        }
    }
}

impl<'s> Iterator for TokenIterator<'s> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.iter.next();
        if let Some(peek) = self.peek(0) {
            (self.last_was_braced, self.start_of_last_token) = (
                matches!(peek.kind, TokenKind::Braces(_)), peek.span.1.end
            );
        }
        self.peek.rotate_left(1);
        std::mem::replace(self.peek.last_mut().unwrap(), next)
    }
}

impl<'s> From<Tokenizer<'s>> for TokenIterator<'s> {
    fn from(value: Tokenizer<'s>) -> Self {
        Self::new(
            value.src,
            value.offset(),
            None,
            value.logger.clone(),
            TokenIterSrc::Tokenizer(value)
        )
    }
}

impl<'s> From<TokenTree<'s>> for TokenIterator<'s> {
    fn from(value: TokenTree<'s>) -> Self {
        TokenIterator::new(
            value.src,
            value.start_offset,
            Some(value.eof.clone()),
            value.logger.clone(),
            TokenIterSrc::Tree(value)
        )
    }
}
