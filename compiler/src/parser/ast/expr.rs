
use std::fmt::Debug;
use strum::IntoEnumIterator;

use crate::{
    parser::{
        node::{Parse, ParseValue, ASTNode, Span},
        stream::{TokenStream, Token},
        ast::token::{Kw, StringLit, IntLit, FloatLit, VoidLit, BoolLit, Ident, Op}
    },
    shared::logging::{Message, Level, Note},
    compiler::{typecheck::{TypeCheck, TypeChecker, Ty, Entity, FindItem}, typehelper::TypeCheckHelper}
};
use super::{
    decls::{VarDecl, FunDecl},
    token::{Path, Parenthesized, Prec, Braced},
    binop::BinOp,
    unop::{UnOp, Call},
    flow::{If, Return}
};

#[derive(Debug)]
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

    VarDecl(VarDecl<'s>),
    FunDecl(FunDecl<'s>),

    If(If<'s>),
    Block(Block<'s>),
    Return(Return<'s>),
}

impl<'s> Expr<'s> {
    fn parse_single<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        if Kw::Var.peek_value(stream) {
            Ok(Self::VarDecl(stream.parse()?))
        }
        else if Kw::Fun.peek_value(stream) {
            Ok(Self::FunDecl(stream.parse()?))
        }
        else if Kw::If.peek_value(stream) {
            Ok(Self::If(stream.parse()?))
        }
        else if Kw::Return.peek_value(stream) {
            Ok(Self::Return(stream.parse()?))
        }
        else if "::".peek_value(stream) || Ident::peek(stream) {
            Ok(Self::Entity(stream.parse()?))
        }
        else {
            let Some(tk) = stream.next() else {
                return Err(stream.error(format!("Expected expression, got {}", stream.whats_eof()), start));
            };
            match tk {
                Token::Void(lit) => Ok(Self::Void(lit)),
                Token::Bool(lit) => Ok(Self::Bool(lit)),
                Token::Int(lit) => Ok(Self::Int(lit)),
                Token::Float(lit) => Ok(Self::Float(lit)),
                Token::String(lit) => Ok(Self::String(lit)),
                tk => Err(stream.error(format!("Expected expression, got {tk}"), start)),
            }
        }
    }

    fn parse_postfix<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let mut expr = Self::parse_single(stream)?;
        loop {
            if Parenthesized::peek(stream) {
                expr = Expr::Call(Call::parse_with(expr, stream)?);
            }
            else {
                break;
            }
        }
        Ok(expr)
    }

    pub fn parse_unop<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        if Op::peek(stream) {
            Ok(Self::UnOp(stream.parse()?))
        }
        else {
            Self::parse_postfix(stream)
        }
    }
    
    fn parse_binop_prec<F>(prec: Prec, sides: &mut F, stream: &mut TokenStream<'s>) -> Result<Self, Message<'s>>
        where
            F: FnMut(&mut TokenStream<'s>) -> Result<Expr<'s>, Message<'s>>
    {
        let mut lhs = sides(stream)?;
        while prec.peek(stream) {
            lhs = Expr::BinOp(BinOp::parse_with(lhs, |s| sides(s), stream)?);
        }
        Ok(lhs)
    }

    fn parse_binop<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let mut sides: Box<dyn FnMut(&mut TokenStream<'s>) -> _> = Box::from(Self::parse_unop);
        for prec in Prec::iter().skip(1) {
            sides = Box::from(
                move |stream: &mut TokenStream<'s>| Self::parse_binop_prec(prec, &mut sides, stream)
            );
        }
        sides(stream)
    }

    pub fn requires_semicolon(&self) -> bool {
        match self {
            Self::If(_) | Self::Block(_) => false,
            Self::FunDecl(f) => f.requires_semicolon(),
            _ => true,
        }
    }
}

impl<'s> Parse<'s> for Expr<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
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
            Self::VarDecl(t) => t.span(),
            Self::FunDecl(t) => t.span(),
            Self::UnOp(t) => t.span(),
            Self::BinOp(t) => t.span(),
            Self::Call(t) => t.span(),
            Self::Block(t) => t.span(),
            Self::If(t) => t.span(),
            Self::Return(t) => t.span(),
        }
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Expr<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        match self {
            Self::Void(_) => Ty::Void,
            Self::Bool(_) => Ty::Bool,
            Self::Int(_) => Ty::Int,
            Self::Float(_) => Ty::Float,
            Self::String(_) => Ty::String,
            Self::Entity(name) => {
                let path = name.path();
                match checker.find::<Entity, _>(&path) {
                    FindItem::Some(e) => e.ty(),
                    FindItem::NotAvailable(e) => {
                        checker.emit_msg(Message::from_span(
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
                        checker.emit_msg(Message::from_span(
                            Level::Error,
                            format!("Unknown entity '{path}'"),
                            name.span(),
                        ));
                        Ty::Invalid
                    }
                }
            }
            Self::UnOp(unop) => unop.typecheck_impl(checker),
            Self::BinOp(binop) => binop.typecheck_impl(checker),
            Self::Call(call) => call.typecheck_impl(checker),
            Self::VarDecl(t) => t.typecheck_impl(checker),
            Self::FunDecl(t) => t.typecheck_impl(checker),
            Self::Block(t) => t.typecheck_impl(checker),
            Self::If(t) => t.typecheck_impl(checker),
            Self::Return(t) => t.typecheck_impl(checker),
        }
    }
}

#[derive(Debug)]
pub struct ExprList<'s> {
    list: Vec<Expr<'s>>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for ExprList<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        // just parse until the stream is over
        // since braces are treated as a single token that result in a substream 
        // which is parsed until the closing brace is found and then returns eof 
        // this works perfectly for all cases
        let mut list = vec![];
        while !stream.is_eof() {
            let expr = Expr::parse(stream)?;
            if expr.requires_semicolon() {
                ";".parse_value(stream)?;
            }
            list.push(expr);
            // parse any amount of extra semicolons and then warn about them
            let semi_start = stream.skip_ws();
            while ";".parse_value(stream).is_ok() {}
            let semi_end = stream.pos();
            if semi_start != semi_end {
                // todo: emit warning
            }
        }
        Ok(Self { list, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for ExprList<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for ExprList<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        self.list.typecheck_helper(checker);
        Ty::Void
    }
}

#[derive(Debug)]
pub struct Block<'s> {
    list: ExprList<'s>,
    span: Span<'s>,
}

impl<'s> Parse<'s> for Block<'s> {
    fn parse_impl<I: Iterator<Item = Token<'s>>>(stream: &mut TokenStream<'s, I>) -> Result<Self, Message<'s>> {
        let start = stream.skip_ws();
        let mut braced = Braced::parse(stream)?.into_stream();
        Ok(Self { list: braced.parse()?, span: stream.span(start) })
    }
}

impl<'s> ASTNode<'s> for Block<'s> {
    fn span(&self) -> &Span<'s> {
        &self.span
    }
}

impl<'s, 'n> TypeCheck<'s, 'n> for Block<'s> {
    fn typecheck_impl(&'n self, checker: &mut TypeChecker<'s, 'n>) -> Ty<'s, 'n> {
        self.list.typecheck_helper(checker);
        Ty::Void
    }
}
