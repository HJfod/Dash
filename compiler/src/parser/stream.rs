
use std::fmt::{Debug, Display};
use crate::shared::{src::Src, logging::Message};
use unicode_xid::UnicodeXID;
use super::{
    node::{Span, ASTNode, Parse},
    ast::token::{
        Kw, Op, Ident, VoidLit, BoolLit, StringLit, IntLit, FloatLit,
        Parenthesized, Braced, Bracketed, Lit,
        is_op_char, closing_paren, Punct
    }
};

// todo: store doc comments for tokens maybe?
// or maybe be evil and have doc comments be tokens that can only appear before items >:3

#[derive(Debug)]
pub enum Token<'s> {
    /// Keyword e.g. `return`
    Kw(Kw<'s>),
    /// Operator e.g. `+=`
    Op(Op<'s>),
    /// Identifier e.g. `ident`
    Ident(Ident<'s>),
    /// Punctuation e.g. `;`, `,`, `...`
    Punct(Punct<'s>),
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
            Token::Kw(kw) => f.write_fmt(format_args!("{kw}")),
            Token::Op(op) => f.write_fmt(format_args!("{op}")),
            Token::Ident(_) => f.write_str("identifier"),
            Token::Punct(p) => f.write_fmt(format_args!("{p}")),
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
            Token::Kw(kw) => kw.span(),
            Token::Op(op) => op.span(),
            Token::Ident(ident) => ident.span(),
            Token::Punct(pun) => pun.span(),
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

    pub fn pos(&self) -> usize {
        self.pos
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
}

impl<'s> Iterator for SrcReader<'s> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        fn get_while<'s, F: FnMut(char) -> bool>(reader: &mut SrcReader<'s>, mut fun: F, res: &mut String) {
            while reader.src.get(reader.pos).is_some_and(|c| fun(c)) {
                res.push(reader.src.get(reader.pos).unwrap());
                reader.pos += 1;
            }
        }
    
        fn skip_ws<'s>(reader: &mut SrcReader<'s>) -> usize {
            loop {
                let Some(ch) = reader.src.get(reader.pos) else { break };
                // Ignore comments
                if ch == '/' && reader.src.get(reader.pos + 1) == Some('/') {
                    reader.pos += 2;
                    while let Some(c) = reader.src.get(reader.pos) {
                        reader.pos += 1;
                        if c == '\n' {
                            break;
                        }
                    }
                    continue;
                }
                if !ch.is_whitespace() {
                    break;
                }
                reader.pos += 1;
            }
            reader.pos
        }
    
        // Skip all whitespace and comments
        skip_ws(self);

        let Some(ch) = self.src.get(self.pos) else {
            // EOF
            return None;
        };
        let next_ch = self.src.get(self.pos);

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
            get_while(self, UnicodeXID::is_xid_continue, &mut res);
            if let Some(kw) = Kw::try_new(res.as_str(), make_span(self.pos)) {
                Some(match kw {
                    Kw::Void(v) => Token::Void(Lit::new((), v.span().to_owned())),
                    Kw::True(t) => Token::Bool(Lit::new(true, t.span().to_owned())),
                    Kw::False(f) => Token::Bool(Lit::new(false, f.span().to_owned())),
                    kw => Token::Kw(kw),
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
            get_while(self, |c| c.is_digit(10) || {
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
        else if matches!(ch, ',' | ';' | '.' | ':') || (matches!(ch, '-' | '=') && next_ch == Some('>')) {
            // Chained punctuation
            let mut res = String::from(ch);
            if matches!(ch, '.' | ':') {
                get_while(self, |c| c == ch, &mut res);
            }
            match Punct::try_new(res.as_str(), make_span(self.pos)) {
                Some(p) => Some(Token::Punct(p)),
                None => make_error(format!("invalid punctuation '{res}'"), self.pos),
            }
        }
        // Operator
        else if is_op_char(ch) {
            let mut res = String::from(ch);
            get_while(self, is_op_char, &mut res);
            match Op::try_new(res.as_str(), make_span(self.pos)) {
                Some(op) => Some(Token::Op(op)),
                None => make_error(format!("invalid operator '{res}'"), self.pos),
            }
        }
        // Parenthesized subtrees
        else if matches!(ch, '(' | '[' | '{') {
            let mut tree = vec![];
            'find_closing: loop {
                // skip whitespace
                skip_ws(self);
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

pub struct TokenStream<'s, I: Iterator<Item = Token<'s>>> {
    src: &'s Src,
    iter: I,
    /// Stored for peeking
    next_token: Option<Token<'s>>,
    eof: Option<(char, Span<'s>)>,
}

pub trait IntoTokenStream<'s>: IntoIterator<Item = Token<'s>> {
    fn src(&self) -> &'s Src;
    fn eof(&self) -> Option<(char, Span<'s>)>;
}

impl<'s, I: IntoTokenStream<'s>> From<I> for TokenStream<'s, I::IntoIter> {
    fn from(value: I) -> Self {
        let src = value.src();
        let eof = value.eof();
        let mut iter = value.into_iter();
        let next_token = iter.next();
        Self { src, iter, next_token, eof }
    }
}

impl<'s, I: Iterator<Item = Token<'s>>> TokenStream<'s, I> {
    pub fn peek(&self) -> Option<Token<'s>> {
        self.next_token
    }

    pub fn parse<P: Parse<'s>>(&mut self) -> Result<P, Message<'s>> {
        P::parse(self)
    }

    pub fn eof_name(&self) -> String {
        match self.eof {
            Some((ch, _)) => String::from(ch),
            None => String::from("EOF"),
        }
    }

    pub fn eof_span(&self) -> Span<'s> {
        match self.eof {
            Some((_, ref span)) => span.clone(),
            None => self.src.range(start, end)
        }
    }
}

impl<'s, I: Iterator<Item = Token<'s>>> Iterator for TokenStream<'s, I> {
    type Item = Token<'s>;
    fn next(&mut self) -> Option<Self::Item> {
        std::mem::replace(&mut self.next_token, self.iter.next())
    }
}
