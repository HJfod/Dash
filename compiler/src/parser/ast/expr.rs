
use std::fmt::Debug;
use dash_macros::{ast_node, impl_opaque};
use strum::IntoEnumIterator;

use crate::{
    parser::{
        node::{Parse, ASTNode, ASTRef},
        stream::{TokenStream, Token},
        ast::token::{StringLit, IntLit, FloatLit, VoidLit, BoolLit, Ident, Op}
    },
    shared::{logging::{Message, Level}, src::Span},
    compiler::{visitor::TakeVisitor, coherency::CoherencyVisitor, ty::Ty}
};
use super::{
    item::{Item, UsingItem},
    token::{Parenthesized, Prec, Braced, self, Tokenize},
    ops::{UnOp, BinOp, Call},
    flow::{If, Return}, Path
};

#[derive(Debug)]
pub enum Visibility {
    Default(Span),
    Public(Span),
    Private(Span),
}

impl ASTNode for Visibility {
    fn span(&self) -> &Span {
        match self {
            Self::Default(span) | Self::Public(span) | Self::Private(span) => span,
        }
    }

    fn children(&mut self) -> Vec<ASTRef> {
        vec![]
    }

    fn eval_ty(&self) -> Ty {
        Ty::Invalid
    }
}

impl Parse for Visibility {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
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
#[impl_opaque {
    impl ASTNode {
        fn span(&self) -> &Span:
            ..e => e.span();
        fn children(&mut self) -> Vec<ASTRef>:
            ..e => e.children();
        fn eval_ty(&self) -> Ty:
            ..e => e.eval_ty();
    }
}]
pub enum Expr {
    Void(VoidLit),
    Bool(BoolLit),
    Int(IntLit),
    Float(FloatLit),
    String(StringLit),
    Entity(Path),

    UnOp(UnOp),
    BinOp(BinOp),
    Call(Call),

    Item(Item),
    UsingItem(UsingItem),

    If(If),
    Block(Block),
    Return(Return),
}

impl Expr {
    fn parse_single<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
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

    fn parse_postfix<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
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

    pub fn parse_unop<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        if Op::peek(stream).is_some() {
            Ok(Self::UnOp(stream.parse()?))
        }
        else {
            Self::parse_postfix(stream)
        }
    }
    
    fn parse_binop_prec<F, I>(prec: Prec, sides: &mut F, stream: &mut TokenStream<I>) -> Result<Self, Message>
        where
            I: Iterator<Item = Token>,
            F: FnMut(&mut TokenStream<I>) -> Result<Expr, Message>
    {
        let mut lhs = sides(stream)?;
        while prec.peek_op_of_this_prec(stream) {
            lhs = Expr::BinOp(BinOp::parse_with(lhs, |s| sides(s), stream)?);
        }
        Ok(lhs)
    }

    fn parse_binop<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let mut sides: Box<dyn FnMut(&mut TokenStream<I>) -> _> = Box::from(Self::parse_unop);
        for prec in Prec::iter().skip(1) {
            sides = Box::from(
                move |stream: &mut TokenStream<I>|
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

impl Parse for Expr {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        Self::parse_binop(stream)
    }
}

impl TakeVisitor<CoherencyVisitor> for Expr {
    fn take_visitor(&mut self, visitor: &mut CoherencyVisitor) {
        match self {
            Self::Entity(name) => {
                todo!()
            }
            _ => ()
        }
    }
}

#[derive(Debug)]
#[ast_node]
pub struct ExprList {
    list: Vec<Expr>,
}

impl Parse for ExprList {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
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
        Ok(Self { list, span: start..stream.pos(), eval_ty: Ty::Unresolved })
    }
}

#[derive(Debug)]
#[ast_node]
pub struct Block {
    list: ExprList,
}

impl Parse for Block {
    fn parse<I: Iterator<Item = Token>>(stream: &mut TokenStream<I>) -> Result<Self, Message> {
        let start = stream.pos();
        let mut braced = Braced::parse(stream)?.into_stream();
        Ok(Self { list: braced.parse()?, span: start..stream.pos(), eval_ty: Ty::Unresolved })
    }
}

pub type AST = ExprList;
