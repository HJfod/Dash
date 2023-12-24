
use parser::tokenizer::{Tokenizer, Token};
use shared::logger::LoggerRef;
use shared::src::Src;

pub mod parser;
pub mod shared;
pub mod ast;

pub fn tokenize<'s, 'g: 's>(src: &'s Src, logger: LoggerRef) -> Vec<Token<'s>> {
    Tokenizer::new(src, logger).collect()
}
