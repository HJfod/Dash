
use std::fmt::Debug;
use dash_macros::ast_node;
use strum::IntoEnumIterator;

use crate::{
    parser::{
        node::{Parse, ASTNode},
        stream::{TokenStream, Token},
        ast::token::{StringLit, IntLit, FloatLit, VoidLit, BoolLit, Ident, Op}
    },
    shared::{logging::{Message, Level, Note}, src::Span},
    compiler::{ty::{TypeVisitor, Ty, Entity, FindItem}, visitor::Visitors}
};
use super::{
    item::{Item, UsingItem},
    token::{Parenthesized, Prec, Braced, self, Tokenize},
    ops::{UnOp, BinOp, Call},
    flow::{If, Return}, Path
};

#[derive(Debug)]
pub enum Visibility<'s> {
    Default(Span<'s>),
    Public(Span<'s>),
    Private(Span<'s>),
}

impl<'s> ASTNode<'s> for Visibility<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::Default(span) | Self::Public(span) | Self::Private(span) => span,
        }
    }
}

impl<'s> Parse<'s> for Visibility<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        if let Some(kw) = token::Public::peek_and_parse(stream) {
            Ok(Self::Public(kw.span().clone()))
        }
        else if let Some(kw) = token::Private::peek_and_parse(stream) {
            Ok(Self::Private(kw.span().clone()))
        }
        else {
            Ok(Self::Default(stream.pos()..stream.pos()))
        }
    }
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum Expr<'s> {
    Void(VoidLit<'s>),
    Bool(BoolLit<'s>),
    Int(IntLit<'s>),
    Float(FloatLit<'s>),
    String(StringLit<'s>),
    Entity(Path<'s>),

    UnOp(UnOp<'s>),
    BinOp(BinOp<'s>),
    Call(Call<'s>),

    Item(Item<'s>),
    UsingItem(UsingItem<'s>),

    If(If<'s>),
    Block(Block<'s>),
    Return(Return<'s>),
}

impl<'s> Expr<'s> {
    fn parse_single<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        if Item::peek(stream) {
            Ok(Self::Item(stream.parse()?))
        }
        else if token::Using::peek(stream).is_some() {
            Ok(Self::UsingItem(stream.parse()?))
        }
        else if token::If::peek(stream).is_some() {
            Ok(Self::If(stream.parse()?))
        }
        else if token::Return::peek(stream).is_some() {
            Ok(Self::Return(stream.parse()?))
        }
        else if token::Dicolon::peek(stream).is_some() || Ident::peek(stream).is_some() {
            Ok(Self::Entity(stream.parse()?))
        }
        else {
            match stream.next() {
                Token::VoidLit(lit) => Ok(Self::Void(lit)),
                Token::BoolLit(lit) => Ok(Self::Bool(lit)),
                Token::IntLit(lit) => Ok(Self::Int(lit)),
                Token::FloatLit(lit) => Ok(Self::Float(lit)),
                Token::StringLit(lit) => Ok(Self::String(lit)),
                tk => Err(Message::from_span(
                    Level::Error,
                    format!("Expected expression, got {tk}"),
                    tk.span()
                )),
            }
        }
    }

    fn parse_postfix<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let mut expr = Self::parse_single(stream)?;
        loop {
            if Parenthesized::peek(stream).is_some() {
                expr = Expr::Call(Call::parse_with(expr, stream)?);
            }
            else {
                break;
            }
        }
        Ok(expr)
    }

    pub fn parse_unop<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        if Op::peek(stream).is_some() {
            Ok(Self::UnOp(stream.parse()?))
        }
        else {
            Self::parse_postfix(stream)
        }
    }
    
    fn parse_binop_prec<F, I>(prec: Prec, sides: &mut F, stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>>
        where
            I: Iterator<Item = Token<'s>>,
            F: FnMut(&mut TokenStream<'s, I>) -> Result<Expr<'s>, Message<'s>>
    {
        let mut lhs = sides(stream)?;
        while prec.peek_op_of_this_prec(stream) {
            lhs = Expr::BinOp(BinOp::parse_with(lhs, |s| sides(s), stream)?);
        }
        Ok(lhs)
    }

    fn parse_binop<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let mut sides: Box<dyn FnMut(&mut TokenStream<'s, I>) -> _> = Box::from(Self::parse_unop);
        for prec in Prec::iter().skip(1) {
            sides = Box::from(
                move |stream: &mut TokenStream<'s, I>|
                    Self::parse_binop_prec(prec, &mut sides, stream)
            );
        }
        sides(stream)
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            Self::If(_) | Self::Block(_) => false,
            Self::Item(f) => f.requires_semicolon(),
            _ => true,
        }
    }
}

impl<'s> Parse<'s> for Expr<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        Self::parse_binop(stream)
    }
}

impl<'s> ASTNode<'s> for Expr<'s> {
    fn span(&self) -> &Span<'s> {
        match self {
            Self::Void(t) => t.span(),
            Self::Bool(t) => t.span(),
            Self::Int(t) => t.span(),
            Self::Float(t) => t.span(),
            Self::String(t) => t.span(),
            Self::Entity(t) => t.span(),
            Self::Item(t) => t.span(),
            Self::UsingItem(t) => t.span(),
            Self::UnOp(t) => t.span(),
            Self::BinOp(t) => t.span(),
            Self::Call(t) => t.span(),
            Self::Block(t) => t.span(),
            Self::If(t) => t.span(),
            Self::Return(t) => t.span(),
        }
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Expr<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        match self {
            Self::Void(_) => Ty::Void,
            Self::Bool(_) => Ty::Bool,
            Self::Int(_) => Ty::Int,
            Self::Float(_) => Ty::Float,
            Self::String(_) => Ty::String,
            Self::Entity(name) => {
                let path = name.path();
                match visitor.find::<Entity, _>(&path) {
                    FindItem::Some(e) => e.ty(),
                    FindItem::NotAvailable(e) => {
                        visitor.emit_msg(Message::from_span(
                            Level::Error,
                            format!("Entity '{path}' can not be used here"),
                            name.span(),
                        ).note(Note::from_span(
                            format!("'{path}' declared here"),
                            e.decl().span()
                        )));
                        Ty::Invalid
                    }
                    FindItem::None => {
                        visitor.emit_msg(Message::from_span(
                            Level::Error,
                            format!("Unknown entity '{path}'"),
                            name.span(),
                        ));
                        Ty::Invalid
                    }
                }
            }
            Self::UnOp(unop) => unop.visit_coherency(visitor),
            Self::BinOp(binop) => binop.visit_coherency(visitor),
            Self::Call(call) => call.visit_coherency(visitor),
            Self::Item(t) => t.visit_coherency(visitor),
            Self::UsingItem(t) => t.visit_coherency(visitor),
            Self::Block(t) => t.visit_coherency(visitor),
            Self::If(t) => t.visit_coherency(visitor),
            Self::Return(t) => t.visit_coherency(visitor),
        }
    }
}

#[derive(Debug)]
#[ast_node]
pub struct ExprList<'s> {
    list: Vec<Expr<'s>>,
}

impl<'s> Parse<'s> for ExprList<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        // just parse until the stream is over
        // since braces are treated as a single token that result in a substream 
        // which is parsed until the closing brace is found and then returns eof 
        // this works perfectly for all cases
        let mut list = vec![];
        while !stream.eof() {
            let expr = Expr::parse(stream)?;
            if expr.requires_semicolon() {
                token::Semicolon::parse(stream)?;
            }
            list.push(expr);
            // parse any amount of extra semicolons and then warn about them
            let semi_start = stream.pos();
            while token::Semicolon::peek_and_parse(stream).is_some() {}
            let semi_end = stream.pos();
            if semi_start != semi_end {
                // todo: emit warning
            }
        }
        Ok(Self { list, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for ExprList<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        self.list.iter().for_each(|v| drop(v.visit_coherency(visitor)));
        Ty::Void
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Block<'s> {
    list: ExprList<'s>,
}

impl<'s> Parse<'s> for Block<'s> {
    fn parse<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.pos();
        let mut braced = Braced::parse(stream)?.into_stream();
        Ok(Self { list: braced.parse()?, span: start..stream.pos() })
    }
}

impl<'s, 'n> Visitors<'s, 'n> for Block<'s> {
    fn visit_coherency(&'n self, visitor: &mut TypeVisitor<'s, 'n>) -> Ty<'s, 'n> {
        self.list.visit_coherency(visitor);
        Ty::Void
    }
}

pub type AST<'s> = ExprList<'s>;
