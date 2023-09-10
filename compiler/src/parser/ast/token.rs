
use std::fmt::{Display, Debug};
use gdml_macros::gdml_log;
use strum::{EnumString, Display, EnumIter};

use crate::{
    parser::{
        node::{Parse, ParseValue, Span, ASTNode},
        stream::{TokenStream, Token}
    },
    shared::logging::{Message, Level}, compiler::typecheck
};

trait ParseToken<'s>: Sized + PartialEq + Display + ASTNode<'s> {
    fn expected_type() -> &'static str;
    fn from_token(tk: Token<'s>) -> Result<Self, Token<'s>>;

    fn parse_token(stream: &mut TokenStream<'s>, expected: String) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let Some(tk) = stream.next() else {
            return Err(stream.error(format!("Expected {expected}, got {}", stream.whats_eof()), start));
        };
        match Self::from_token(tk) {
            Ok(t) => Ok(t),
            Err(tk) => Err(stream.error(format!("Expected {expected}, got {tk}"), start)),
        }
    }
}

impl<'s, T: ParseToken<'s>> Parse<'s> for T {
    fn parse_impl(stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        Self::parse_token(stream, Self::expected_type().into())
    }
}

impl<'s, T: ParseToken<'s>> ParseValue<'s> for T {
    fn parse_value_impl(self, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let tk = Self::parse_token(stream, format!("'{}'", self.to_string()))?;
        if tk == self {
            Ok(self)
        }
        else {
            Err(stream.error(format!("Expected '{self}', got {tk}"), start))
        }
    }
}

pub fn is_op_char(ch: char) -> bool {
    "=+-/%&|^*~@!?<>#".contains(ch)
}

pub fn closing_paren(ch: char) -> char {
    match ch {
        '(' => ')',
        '[' => ']',
        '{' => '}',
        _   => unreachable!(),
    }
}

/// A keyword token in GDML
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, EnumString, Display)]
#[strum(serialize_all = "snake_case")]
pub enum Kw {
    Void, True, False,
    Var, Let, Fun, Decl, Struct, Const,
    Is, As, From,
    If, Else, For, While,
    Return, Break,
    Extern, Export, Import,
    Yield, Match, Switch, Mut,
}

const FIRST_STRICT_KW: Kw = Kw::Var;
const FIRST_CONTEXTUAL_KW: Kw = Kw::Yield;
const FIRST_RESERVED_KW: Kw = Kw::Yield;

impl Kw {
    /// Whether this keyword is strict, i.e. not allowed to be used in 
    /// identifiers
    pub fn is_strict(&self) -> bool {
        *self >= FIRST_STRICT_KW && *self < FIRST_CONTEXTUAL_KW
    }

    /// Whether this keyword is contextual, i.e. allowed to be used in 
    /// identifiers
    pub fn is_contextual(&self) -> bool {
        *self >= FIRST_CONTEXTUAL_KW && *self < FIRST_RESERVED_KW
    }

    /// Whether this keyword is reserved, i.e. not allowed to be used 
    /// anywhere
    pub fn is_reserved(&self) -> bool {
        *self >= FIRST_RESERVED_KW
    }
}

impl<'s> ASTNode<'s> for Kw {
    fn span(&self) -> &Span<'s> {
        panic!("cannot get span of keyword")
    }
}

impl<'s> ParseToken<'s> for Kw {
    fn expected_type() -> &'static str {
        "keyword"
    }

    fn from_token(tk: Token<'s>) -> Result<Self, Token<'s>> {
        match tk {
            Token::Kw(kw, _) => Ok(kw),
            other => Err(other),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, EnumIter)]
pub enum Prec {
    Unary,
    Mul,
    Add,
    Ord,
    Eq,
    And,
    Or,
    Seq,
}

impl Prec {
    pub fn peek(&self, stream: &mut TokenStream) -> bool {
        let start = stream.pos();
        if let Ok(op) = Op::parse(stream) {
            stream.goto(start);
            op.prec() == *self
        }
        else {
            stream.goto(start);
            false
        }
    }
}

/// An operator token in GDML
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, EnumString, Display)]
pub enum Op {
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Sub,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
    #[strum(serialize = "%")]
    Mod,

    #[strum(serialize = "=")]
    Seq,
    #[strum(serialize = "+=")]
    AddSeq,
    #[strum(serialize = "-=")]
    SubSeq,
    #[strum(serialize = "*=")]
    MulSeq,
    #[strum(serialize = "/=")]
    DivSeq,
    #[strum(serialize = "%=")]
    ModSeq,
    
    #[strum(serialize = "==")]
    Eq,
    #[strum(serialize = "!=")]
    Neq,
    #[strum(serialize = "<")]
    Lss,
    #[strum(serialize = ">")]
    Gtr,
    #[strum(serialize = "<=")]
    Leq,
    #[strum(serialize = ">=")]
    Geq,
    
    #[strum(serialize = "&&")]
    And,
    #[strum(serialize = "||")]
    Or,
    
    #[strum(serialize = "!")]
    Not,
}

impl Op {
    pub fn prec(&self) -> Prec {
        match self {
            Op::Not => Prec::Unary,
            Op::Mul | Op::Div | Op::Mod => Prec::Mul,
            Op::Add | Op::Sub => Prec::Add,
            Op::Lss | Op::Gtr | Op::Leq | Op::Geq => Prec::Ord,
            Op::Eq | Op::Neq => Prec::Eq,
            Op::And => Prec::And,
            Op::Or => Prec::Or,
            Op::Seq | Op::AddSeq | Op::SubSeq | Op::MulSeq | Op::DivSeq | Op::ModSeq => Prec::Seq,
        }
    }
}

impl<'s> ASTNode<'s> for Op {
    fn span(&self) -> &Span<'s> {
        panic!("cannot get span of operator")
    }
}

impl<'s> ParseToken<'s> for Op {
    fn expected_type() -> &'static str {
        "operator"
    }

    fn from_token(tk: Token<'s>) -> Result<Self, Token<'s>> {
        match tk {
            Token::Op(op, _) => Ok(op),
            other => Err(other),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Ident<'s>(String, Span<'s>);

impl<'s> Ident<'s> {
    pub fn new(value: String, span: Span<'s>) -> Self {
        Self(value, span)
    }

    pub fn is_keyword(&self) -> bool {
        Kw::try_from(self.0.as_str()).is_ok()
    }

    pub fn value(&self) -> &String {
        &self.0
    }

    pub fn path(&self) -> typecheck::Path {
        typecheck::Path::new([self.0.clone()], false)
    }
}

impl Display for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<'s> ASTNode<'s> for Ident<'s> {
    fn span(&self) -> &Span<'s> {
        &self.1
    }
}

impl<'s> ParseToken<'s> for Ident<'s> {
    fn expected_type() -> &'static str {
        "identifier"
    }

    fn from_token(tk: Token<'s>) -> Result<Self, Token<'s>> {
        match tk {
            Token::Ident(i) => Ok(i),
            other => Err(other),
        }
    }
}

#[derive(Debug)]
pub struct Lit<'s, T: Debug> {
    value: T,
    span: Span<'s>,
}

impl<'s, T: Debug> Lit<'s, T> {
    pub fn new(value: T, span: Span<'s>) -> Self {
        Self { value, span }
    }
}

impl<'s, T: Debug> ASTNode<'s> for Lit<'s, T> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

pub type VoidLit<'s> = Lit<'s, ()>;
pub type BoolLit<'s> = Lit<'s, bool>;
pub type IntLit<'s> = Lit<'s, i64>;
pub type FloatLit<'s> = Lit<'s, f64>;
pub type StringLit<'s> = Lit<'s, String>;

#[derive(Debug)]
pub struct Parenthesized<'s> {
    substream: TokenStream<'s>,
    span: Span<'s>,
}

impl<'s> Parenthesized<'s> {
    pub fn new(substream: TokenStream<'s>, span: Span<'s>) -> Self {
        Self { substream, span }
    }

    pub fn into_stream(self) -> TokenStream<'s> {
        self.substream
    }
}

impl<'s> Parse<'s> for Parenthesized<'s> {
    fn parse_impl(stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        match stream.next() {
            Some(Token::Parenthesized(p)) => Ok(p),
            Some(other) => Err(Message::from_span(
                Level::Error,
                format!("Expected parenthesized expression, got {other}"),
                other.span()
            )),
            None => Err(stream.error(format!("Expected parenthesized expression, got {}", stream.whats_eof()), start))
        }
    }
}

impl<'s> ASTNode<'s> for Parenthesized<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

#[derive(Debug)]
pub struct Bracketed<'s> {
    substream: TokenStream<'s>,
    span: Span<'s>,
}

impl<'s> Bracketed<'s> {
    pub fn new(substream: TokenStream<'s>, span: Span<'s>) -> Self {
        Self { substream, span }
    }

    pub fn into_stream(self) -> TokenStream<'s> {
        self.substream
    }
}

impl<'s> Parse<'s> for Bracketed<'s> {
    fn parse_impl(stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        match stream.next() {
            Some(Token::Bracketed(p)) => Ok(p),
            Some(other) => Err(Message::from_span(
                Level::Error,
                format!("Expected bracketed expression, got {other}"),
                other.span()
            )),
            None => Err(stream.error(format!("Expected bracketed expression, got {}", stream.whats_eof()), start))
        }
    }
}

impl<'s> ASTNode<'s> for Bracketed<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

#[derive(Debug)]
pub struct Braced<'s> {
    substream: TokenStream<'s>,
    span: Span<'s>,
}

impl<'s> Braced<'s> {
    pub fn new(substream: TokenStream<'s>, span: Span<'s>) -> Self {
        Self { substream, span }
    }

    pub fn into_stream(self) -> TokenStream<'s> {
        self.substream
    }
}

impl<'s> Parse<'s> for Braced<'s> {
    fn parse_impl(stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        match stream.next() {
            Some(Token::Braced(p)) => Ok(p),
            Some(other) => Err(Message::from_span(
                Level::Error,
                format!("Expected braced expression, got {other}"),
                other.span()
            )),
            None => Err(stream.error(format!("Expected braced expression, got {}", stream.whats_eof()), start))
        }
    }
}

impl<'s> ASTNode<'s> for Braced<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s> ParseValue<'s> for &'static str {
    fn parse_value_impl(self, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let Some(tk) = stream.next() else {
            return Err(stream.error(format!("Expected '{self}', got {}", stream.whats_eof()), start));
        };
        let value = match tk {
            Token::Punct(ref p, _) => p,
            _ => return Err(stream.error(format!("Expected '{self}', got {tk}"), start)),
        };
        if value == self {
            Ok(self)
        }
        else {
            Err(stream.error(format!("Expected '{self}', got {tk}"), start))
        }
    }
}

#[derive(Debug)]
pub struct Path<'s> {
    components: Vec<Ident<'s>>,
    absolute: bool,
    span: Span<'s>,
}

impl<'s> Path<'s> {
    pub fn path(&self) -> typecheck::Path {
        typecheck::Path::new(
            self.components.iter().map(|i| i.value().clone()).collect::<Vec<_>>(),
            self.absolute
        )
    }
}

impl<'s> Parse<'s> for Path<'s> {
    fn parse_impl(stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        let absolute = "::".parse_value(stream).is_ok();
        let mut components = vec![];
        loop {
            components.push(stream.parse()?);
            if "::".parse_value(stream).is_err() {
                break;
            }
        }
        Ok(Self { components, absolute, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for Path<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}
