
#![warn(clippy::todo)]

use checker::coherency::Checker;
use checker::pool::AST;
use checker::ty::Ty;
use parser::parse::NodePool;
use parser::tokenizer::{Tokenizer, Token};
use shared::logger::LoggerRef;
use shared::src::Src;

pub mod parser;
pub mod shared;
pub mod ast;
pub mod checker;

pub fn tokenize<'s, 'g: 's>(src: &'s Src, logger: LoggerRef) -> Vec<Token<'s>> {
    Tokenizer::new(src, logger).collect()
}

pub fn check_coherency(ast: &mut AST, list: &mut NodePool, logger: LoggerRef) -> Ty {
    Checker::try_resolve(ast, list, logger)
}
