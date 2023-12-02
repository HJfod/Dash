
pub(crate) mod grammar;
pub(crate) mod parse;
pub(crate) mod tokenizer;

#[derive(Default, Clone)]
pub struct ParseOptions {
    pub debug_log_matches: bool,
}
