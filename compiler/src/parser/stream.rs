
use std::{fmt::{Debug, Display}, rc::Rc};
use crate::shared::{src::{Src, Span, Loc}, logging::{Message, LoggerRef, Level}};
use unicode_xid::UnicodeXID;
use super::{
    node::{ASTNode, Parse},
    ast::token::{
        Kw, Op, Ident, VoidLit, BoolLit, StringLit, IntLit, FloatLit,
        Parenthesized, Braced, Bracketed,
        is_op_char, closing_paren, Punct
    }
};

// todo: store doc comments for tokens maybe?
// or maybe be evil and have doc comments be tokens that can only appear before items >:3

#[derive(Clone, Debug)]
#[impl_opaque {
    impl ASTNode {
        fn span(&self) -> &Span:
            Error(_, span) => span;
            EOF(_, span) => span;
            ..e => e.span();
        fn iter_children(&mut self) -> impl Iterator<Item = &mut dyn ASTNode>:
            ..e => e.iter_children();
        fn eval_ty(&self) -> Ty:
            ..e => e.eval_ty()
    }
}]
pub enum Token {
    /// Keyword e.g. `return`
    Kw(Kw),
    /// Operator e.g. `+=`
    Op(Op),
    /// Identifier e.g. `ident`
    Ident(Ident),
    /// Punctuation e.g. `;`, `,`, `...`
    Punct(Punct),
    /// Void literal `void`
    VoidLit(VoidLit),
    /// Bool literal e.g. `true`, `false`
    BoolLit(BoolLit),
    /// String literal e.g. `"Example literal"`
    StringLit(StringLit),
    /// Integer literal e.g. `18`
    IntLit(IntLit),
    /// Float literal e.g. `2.3`
    FloatLit(FloatLit),
    /// Parenthesized list of tokens e.g. `(...)`
    Parenthesized(Parenthesized),
    /// Braced list of tokens e.g. `{...}`
    Braced(Braced),
    /// Bracketed list of tokens e.g. `[...]`
    Bracketed(Bracketed),
    /// An invalid token
    Error(String, Span),
    /// End-of-file
    EOF(String, Span),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Kw(kw) => f.write_fmt(format_args!("{kw}")),
            Token::Op(op) => f.write_fmt(format_args!("{op}")),
            Token::Ident(i) => f.write_fmt(format_args!("{i}")),
            Token::Punct(p) => f.write_fmt(format_args!("{p}")),
            Token::VoidLit(l) => f.write_fmt(format_args!("{l}")),
            Token::BoolLit(l) => f.write_fmt(format_args!("{l}")),
            Token::StringLit(l) => f.write_fmt(format_args!("{l}")),
            Token::IntLit(l) => f.write_fmt(format_args!("{l}")),
            Token::FloatLit(l) => f.write_fmt(format_args!("{l}")),
            Token::Parenthesized(p) => f.write_fmt(format_args!("{p}")),
            Token::Braced(b) => f.write_fmt(format_args!("{b}")),
            Token::Bracketed(b) => f.write_fmt(format_args!("{b}")),
            Token::Error(msg, _) => f.write_fmt(format_args!("invalid token: {msg}")),
            Token::EOF(name, _) => f.write_str(name)
        }
    }
}

impl ASTNode for Token {
    fn span(&self) -> &Span {
        match self {
            Token::Kw(kw) => kw.span(),
            Token::Op(op) => op.span(),
            Token::Ident(ident) => ident.span(),
            Token::Punct(pun) => pun.span(),
            Token::VoidLit(lit) => lit.span(),
            Token::BoolLit(lit) => lit.span(),
            Token::StringLit(lit) => lit.span(),
            Token::IntLit(lit) => lit.span(),
            Token::FloatLit(lit) => lit.span(),
            Token::Parenthesized(p) => p.span(),
            Token::Braced(p) => p.span(),
            Token::Bracketed(p) => p.span(),
            Token::Error(_, span) => span,
            Token::EOF(_, span) => span,
        }
    }

    fn iter_children(&mut self) -> impl Iterator<Item = &mut dyn ASTNode> {
        
    }

    fn eval_ty(&self) -> crate::compiler::ty::Ty {
        match self {
            Token::Kw(kw) => kw.eval_ty(),
            Token::Op(op) => op.eval_ty(),
            Token::Ident(ident) => ident.eval_ty()
        }
    }
}

// todo: have subtrees be a Vec of parsed tokens instead and make the SrcReader
// parameter in `parse` be just an iterator that produces tokens

/// Parses a source into a list of tokens
#[derive(Debug)]
pub struct SrcReader {
    src: Rc<Src>,
    pos: usize,
    logger: LoggerRef,
}

impl SrcReader {
    pub fn new(src: Rc<Src>, logger: LoggerRef) -> Self {
        Self { src, pos: 0, logger }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }
}

impl Iterator for SrcReader {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        fn get_while<F: FnMut(char) -> bool>(reader: &mut SrcReader, mut fun: F, res: &mut String) {
            while reader.src.get(reader.pos).is_some_and(&mut fun) {
                res.push(reader.src.get(reader.pos).unwrap());
                reader.pos += 1;
            }
        }
    
        fn skip_ws(reader: &mut SrcReader) -> usize {
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
    
        // Stored for EOF
        let pre_ws_pos = self.pos;

        // Skip all whitespace and comments
        skip_ws(self);

        let Some(ch) = self.src.get(self.pos) else {
            // EOF
            return Some(Token::EOF(
                "end-of-file".into(), 
                self.src.span(pre_ws_pos, pre_ws_pos)
            ));
        };

        let first_pos = self.pos;
        self.pos += 1;

        let next_ch = self.src.get(self.pos);

        let make_span = |pos| self.src.span(first_pos, pos);
        let make_error = |msg, pos| Some(Token::Error(msg, make_span(pos)));

        // Identifier or keyword
        if ch.is_xid_start() || (ch == '@' && next_ch.is_some_and(|c| c.is_xid_start())) {
            let mut res = 
                if ch == '@' {
                    // the decorator symbol is not part of the identifier
                    self.pos += 1;
                    String::from(next_ch.unwrap())
                }
                else {
                    String::from(ch)
                };
            get_while(self, UnicodeXID::is_xid_continue, &mut res);
            if let Some(kw) = Kw::try_new(res.as_str(), make_span(self.pos)) {
                Some(match kw {
                    Kw::Void(v) => Token::VoidLit(VoidLit::new(v.span().to_owned())),
                    Kw::True(t) => Token::BoolLit(BoolLit::new(true, t.span().to_owned())),
                    Kw::False(f) => Token::BoolLit(BoolLit::new(false, f.span().to_owned())),
                    kw => Token::Kw(kw),
                })
            }
            else {
                Some(Token::Ident(Ident::new(res, ch == '@', make_span(self.pos))))
            }
        }
        // Number
        else if ch.is_ascii_digit() {
            let mut res = String::from(ch);
            let mut found_dot = false;
            get_while(self, |c| c.is_ascii_digit() || {
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
                    Ok(num) => Some(Token::FloatLit(FloatLit::new(num, make_span(self.pos)))),
                    Err(e) => make_error(format!("invalid float ({e})"), self.pos),
                }
            }
            else {
                match res.parse::<i64>() {
                    Ok(num) => Some(Token::IntLit(IntLit::new(num, make_span(self.pos)))),
                    Err(e) => make_error(format!("invalid float ({e})"), self.pos),
                }
            }
        }
        // String literal
        else if ch == '"' {
            let mut res = String::new();
            while match self.src.get(self.pos) {
                Some('"') => {
                    self.pos += 1;
                    false
                }
                Some(c) => {
                    res.push(match c {
                        '\\' => {
                            self.pos += 1;
                            match self.src.get(self.pos) {
                                Some('n')  => '\n',
                                Some('t')  => '\t',
                                Some('0')  => '\0',
                                Some('r')  => '\r',
                                Some('\\') => '\\',
                                Some('\"') => '\"',
                                Some('\'') => '\'',
                                Some(c) => {
                                    self.logger.lock().unwrap().log_msg(Message::from_span(
                                        Level::Warning,
                                        format!("Invalid escape sequence '\\{c}'"),
                                        &self.src.span(self.pos - 1, self.pos + 1)
                                    ));
                                    c
                                }
                                None => {
                                    self.logger.lock().unwrap().log_msg(Message::from_span(
                                        Level::Warning,
                                        "Expected escape sequence",
                                        &self.src.span(self.pos - 1, self.pos)
                                    ));
                                    '\\'
                                }
                            }
                        }
                        o => o
                    });
                    self.pos += 1;
                    true
                }
                None => false
            } {}
            Some(Token::StringLit(StringLit::new(res, make_span(self.pos))))
        }
        // Single punctuation
        else if matches!(ch, ',' | ';' | '.' | ':') || (matches!(ch, '-' | '=') && next_ch == Some('>')) {
            let mut res = String::from(ch);
            // Chained punctuation
            if matches!(ch, '.' | ':') {
                get_while(self, |c| c == ch, &mut res);
            }
            // Arrows
            if matches!(ch, '-' | '=') {
                // Already checked this in the condition
                res.push('>');
                self.pos += 1;
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
                // push next token to subtree
                tree.push(self.next().unwrap());
            }
            // push eof at the end of the tree (important!)
            tree.push(Token::EOF(
                format!("'{}'", closing_paren(ch)),
                self.src.loc(self.pos)..self.src.loc(self.pos)
            ));
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

pub struct TokenStream<I: Iterator<Item = Token>> {
    src: Rc<Src>,
    iter: I,
    /// Stored for peeking
    next_token: Token,
}

impl<I: Iterator<Item = Token>> TokenStream<I> {
    pub fn new(src: Rc<Src>, mut iter: I) -> Self {
        let next_token = iter.next().expect("IntoTokenStream::next() returned None");
        Self { src, iter, next_token }
    }

    pub fn src(&self) -> Rc<Src> {
        self.src
    }

    pub fn pos(&self) -> Loc {
        self.next_token.span().start
    }

    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Token {
        std::mem::replace(
            &mut self.next_token,
            self.iter.next().expect("TokenStream::next() returned None")
        )
    }

    pub fn peek(&self) -> Token {
        self.next_token.clone()
    }

    pub fn eof(&self) -> bool {
        matches!(self.next_token, Token::EOF(_, _))
    }

    pub fn parse<P: Parse>(&mut self) -> Result<P, Message> {
        P::parse(self)
    }
}
