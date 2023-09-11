
use std::fmt::{Debug, Display};
use crate::shared::{src::Src, logging::{Message, Level}};
use unicode_xid::UnicodeXID;
use super::{
    node::{Span, ASTNode, Parse},
    ast::token::{
        Kw, Op, Ident, VoidLit, BoolLit, StringLit, IntLit, FloatLit,
        Parenthesized, Braced, Bracketed, Lit,
        is_op_char, closing_paren
    }
};

// todo: store doc comments for tokens maybe?
// or maybe be evil and have doc comments be tokens that can only appear before items >:3

#[derive(Debug)]
pub enum Token<'s> {
    /// Keyword e.g. `return`
    Kw(Kw, Span<'s>),
    /// Operator e.g. `+=`
    Op(Op, Span<'s>),
    /// Identifier e.g. `ident`
    Ident(Ident<'s>),
    /// Punctuation e.g. `;`, `,`, `...`
    Punct(String, Span<'s>),
    /// Void literal `void`
    Void(VoidLit<'s>),
    /// Void literal e.g. `true`, `false`
    Bool(BoolLit<'s>),
    /// String literal e.g. `"Example literal"`
    String(StringLit<'s>),
    /// Integer literal e.g. `18`
    Int(IntLit<'s>),
    /// Float literal e.g. `2.3`
    Float(FloatLit<'s>),
    /// Parenthesized list of tokens e.g. `(...)`
    Parenthesized(Parenthesized<'s>),
    /// Braced list of tokens e.g. `{...}`
    Braced(Braced<'s>),
    /// Bracketed list of tokens e.g. `[...]`
    Bracketed(Bracketed<'s>),
    /// An invalid token
    Error(String, Span<'s>),
}

impl<'s> Display for Token<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Kw(kw, _) => f.write_fmt(format_args!("keyword '{kw}'")),
            Token::Op(op, _) => f.write_fmt(format_args!("operator '{op}'")),
            Token::Ident(_) => f.write_str("identifier"),
            Token::Punct(p, _) => f.write_fmt(format_args!("'{p}'")),
            Token::Void(_) => f.write_str("void literal"),
            Token::Bool(_) => f.write_str("boolean literal"),
            Token::String(_) => f.write_str("string literal"),
            Token::Int(_) => f.write_str("integer literal"),
            Token::Float(_) => f.write_str("float literal"),
            Token::Parenthesized(_) => f.write_str("parentheses"),
            Token::Braced(_) => f.write_str("braces"),
            Token::Bracketed(_) => f.write_str("brackets"),
            Token::Error(msg, _) => f.write_fmt(format_args!("invalid token: {msg}")),
        }
    }
}

impl<'s> ASTNode<'s> for Token<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Token::Kw(_, span) => span,
            Token::Op(_, span) => span,
            Token::Ident(ident) => ident.span(),
            Token::Punct(_, span) => span,
            Token::Void(lit) => lit.span(),
            Token::Bool(lit) => lit.span(),
            Token::String(lit) => lit.span(),
            Token::Int(lit) => lit.span(),
            Token::Float(lit) => lit.span(),
            Token::Parenthesized(p) => p.span(),
            Token::Braced(p) => p.span(),
            Token::Bracketed(p) => p.span(),
            Token::Error(_, span) => span,
        }
    }
}

// todo: have subtrees be a Vec of parsed tokens instead and make the SrcReader<'s>
// parameter in `parse` be just an iterator that produces tokens

/// Parses a source into a list of tokens
#[derive(Debug)]
pub struct SrcReader<'s> {
    src: &'s Src,
    pos: usize,
}

impl<'s> SrcReader<'s> {
    pub fn new(src: &'s Src) -> Self {
        Self { src, pos: 0 }
    }

    fn get_while<F: FnMut(char) -> bool>(&mut self, mut fun: F, res: &mut String) {
        while self.src.get(self.pos).is_some_and(|c| fun(c)) {
            res.push(self.src.get(self.pos).unwrap());
            self.pos += 1;
        }
    }

    pub fn skip_ws(&mut self) -> usize {
        loop {
            let Some(ch) = self.src.get(self.pos) else { break };
            // Ignore comments
            if ch == '/' && self.src.get(self.pos + 1) == Some('/') {
                self.pos += 2;
                while let Some(c) = self.src.get(self.pos) {
                    self.pos += 1;
                    if c == '\n' {
                        break;
                    }
                }
                continue;
            }
            if !ch.is_whitespace() {
                break;
            }
            self.pos += 1;
        }
        self.pos
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn goto(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn last_nws_pos(&self) -> usize {
        let mut i = 1;
        loop {
            if self.pos < i {
                return 0;
            }
            let ch = self.src.get(self.pos - i);
            if ch.is_some_and(|c| c.is_whitespace()) {
                i += 1;
                continue;
            }
            break self.pos - i;
        }
    }

    pub fn span(&self, start: usize) -> Span<'s> {
        Span {
            src: self.src,
            range: self.src.range(start, self.last_nws_pos() + 1),
        }
    }

    pub fn error<S: Into<String>>(&self, msg: S, start: usize) -> Message<'s> {
        Message {
            level: Level::Error,
            info: msg.into(),
            notes: vec![],
            src: self.src,
            range: self.src.range(start, self.pos)
        }
    }

    pub fn is_eof(&mut self) -> bool {
        self.skip_ws();
        self.expect_eof().is_ok()
    }

    pub fn expect_eof(&mut self) -> Result<(), Message<'s>> {
        let pos = self.pos;
        match self.next() {
            Some(tk) => {
                self.goto(pos);
                if let Some(ch) = self.eof_char {
                    Err(Message::from_span(
                        Level::Error,
                        format!("Expected '{ch}', got {tk}"),
                        tk.span()
                    ))
                }
                else {
                    Err(Message::from_span(
                        Level::Error,
                        format!("Expected EOF, got {tk}"),
                        tk.span()
                    ))
                }
            }
            None => Ok(())
        }
    }

    pub fn whats_eof(&self) -> String {
        match self.eof_char {
            Some(c) => format!("'{c}'"),
            None => String::from("EOF"),
        }
    }

    /// Parse some type on this stream.
    /// Provided for convenience since this lets you do type inference
    pub fn parse<P: Parse<'s>>(&mut self) -> Result<P, Message<'s>> {
        P::parse(self)
    }
}

impl<'s> Iterator for SrcReader<'s> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        // Skip all whitespace and comments
        self.skip_ws();

        let Some(ch) = self.src.get(self.pos) else {
            // EOF
            return None;
        };

        // EOF
        if self.eof_char == Some(ch) {
            return None;
        }

        let first_pos = self.pos;
        self.pos += 1;

        let make_span = |pos| {
            Span {
                src: self.src,
                range: self.src.range(first_pos, pos)
            }
        };
        let make_error = |msg, pos| {
            Some(Token::Error(msg, make_span(pos)))
        };

        // Identifier or keyword
        if ch.is_xid_start() {
            let mut res = String::from(ch);
            self.get_while(UnicodeXID::is_xid_continue, &mut res);
            if let Ok(kw) = Kw::try_from(res.as_str()) {
                Some(match kw {
                    Kw::Void => Token::Void(Lit::new((), make_span(self.pos))),
                    Kw::True => Token::Bool(Lit::new(true, make_span(self.pos))),
                    Kw::False => Token::Bool(Lit::new(false, make_span(self.pos))),
                    kw => Token::Kw(kw, make_span(self.pos)),
                })
            }
            else {
                Some(Token::Ident(Ident::new(res, make_span(self.pos))))
            }
        }
        // Number
        else if ch.is_digit(10) {
            let mut res = String::from(ch);
            let mut found_dot = false;
            self.get_while(|c| c.is_digit(10) || {
                if c == '.' {
                    if found_dot {
                        false
                    }
                    else {
                        found_dot = true;
                        true
                    }
                }
                else {
                    false
                }
            }, &mut res);
            if found_dot {
                match res.parse::<f64>() {
                    Ok(num) => Some(Token::Float(Lit::new(num, make_span(self.pos)))),
                    Err(e) => make_error(format!("invalid float ({e})"), self.pos),
                }
            }
            else {
                match res.parse::<i64>() {
                    Ok(num) => Some(Token::Int(Lit::new(num, make_span(self.pos)))),
                    Err(e) => make_error(format!("invalid float ({e})"), self.pos),
                }
            }
        }
        // Single punctuation
        else if matches!(ch, ',' | ';') {
            Some(Token::Punct(ch.to_string(), make_span(self.pos)))
        }
        // Chained punctuation
        else if matches!(ch, '.' | ':') {
            let mut res = String::from(ch);
            self.get_while(|c| c == ch, &mut res);
            Some(Token::Punct(res, make_span(self.pos)))
        }
        // Arrows
        else if matches!(ch, '-' | '=') && self.src.get(self.pos) == Some('>') {
            self.pos += 1;
            Some(Token::Punct(format!("{ch}>"), make_span(self.pos)))
        }
        // Operator
        else if is_op_char(ch) {
            let mut res = String::from(ch);
            self.get_while(is_op_char, &mut res);
            if let Ok(op) = Op::try_from(res.as_str()) {
                Some(Token::Op(op, make_span(self.pos)))
            }
            else {
                make_error(format!("invalid operator '{res}'"), self.pos)
            }
        }
        // Parenthesized subtrees
        else if matches!(ch, '(' | '[' | '{') {
            let mut tree = vec![];
            'find_closing: loop {
                // skip whitespace
                self.skip_ws();
                match self.src.get(self.pos) {
                    Some(c) if c == closing_paren(ch) => {
                        // consume closing parenthesis
                        self.pos += 1;
                        break 'find_closing;
                    }
                    Some(_) => {}
                    None => {
                        return make_error(match ch {
                            '(' => "unclosed parenthesis",
                            '{' => "unclosed brace",
                            '[' => "unclosed bracket",
                            _ => unreachable!(),
                        }.into(), first_pos);
                    }
                }
                // get next token
                self.next();
            }
            match ch {
                '(' => Some(Token::Parenthesized(Parenthesized::new(tree, make_span(self.pos)))),
                '{' => Some(Token::Braced(Braced::new(tree, make_span(self.pos)))),
                '[' => Some(Token::Bracketed(Bracketed::new(tree, make_span(self.pos)))),
                _   => unreachable!(),
            }
        }
        else {
            make_error(format!("invalid symbol '{ch}'"), self.pos)
        }
    }
}

pub trait Forkable {
    fn fork(&self) -> Self;
}

impl<'s> Forkable for SrcReader<'s> {
    fn fork(&self) -> Self {
        Self { src: self.src, pos: self.pos }
    }
}

pub trait TokenStream<'s>: IntoIterator<Item = Token<'s>> + Forkable + Sized {
    fn parse<P: Parse<'s>>(&mut self) -> Result<P, Message<'s>> {
        P::parse(self)
    }
}

#[test]
fn verify_kws() {
    use std::str::FromStr;
    assert_eq!(Kw::Var.to_string(), "var");
    assert_eq!(Kw::Return.to_string(), "return");
    assert_eq!(Kw::from_str("yield").unwrap(), Kw::Yield);
    assert_eq!(Kw::Let.is_strict(), true);
    assert_eq!(Kw::Return.is_reserved(), false);
}

#[test]
fn verify_ops() {
    use std::str::FromStr;
    assert_eq!(Op::Eq.to_string(), "==");
    assert_eq!(Op::And.to_string(), "&&");
    assert_eq!(Op::from_str(">").unwrap(), Op::Gtr);
    assert_eq!(Op::from_str("||").unwrap(), Op::Or);
}
