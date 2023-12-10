
use checker::ast::Node;
use checker::coherency::Checker;
use parser::grammar::GrammarFile;
use parser::tokenizer::{Tokenizer, Token};
use shared::logger::LoggerRef;
use shared::src::Src;

pub mod parser;
pub mod shared;
pub mod checker;

// pub(crate) static mut DEBUG_LOG_INDENT: usize = 0;

static DEFAULT_GRAMMAR: &str = include_str!("../grammar/output.combined.json");

pub fn default_grammar() -> GrammarFile<'static> {
    serde_json::from_str(DEFAULT_GRAMMAR).expect("Unable to parse built-in grammar")
}

pub fn tokenize<'s, 'g: 's>(src: &'s Src, grammar: &'s GrammarFile<'g>, logger: LoggerRef) -> Vec<Token<'s>> {
    Tokenizer::new(src, grammar, logger).collect()
}

pub fn check_coherency(node: &mut Node, logger: LoggerRef) {
    let mut checker = Checker::new(logger.clone());
    node.check_coherency(&mut checker, logger.clone());
}
