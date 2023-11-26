
use dash_macros::include_grammar_jsons;
use grammar::GrammarFile;
use logger::LoggerRef;
use src::Src;
use tokenizer::{Tokenizer, Token};

mod grammar;
mod tokenizer;
mod char_iter;
pub mod src;
pub mod ast;
mod parse;
pub mod logger;

static DEFAULT_GRAMMAR: &str = include_grammar_jsons!("../grammar");

pub fn default_grammar() -> GrammarFile<'static> {
    serde_json::from_str(DEFAULT_GRAMMAR).expect("Unable to parse built-in grammar")
}

pub fn tokenize<'s, 'g: 's>(src: &'s Src, grammar: &'s GrammarFile<'g>, logger: LoggerRef) -> Vec<Token<'s>> {
    Tokenizer::new(src, grammar, logger).collect()
}
