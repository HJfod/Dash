
use std::sync::Arc;
use crate::shared::src::Src;
use super::tokenizer::{TokenIterator, Token};

pub trait Parse: Sized {
    /// Parse this struct from the token stream
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
        where I: Iterator<Item = Token<'s>>;
        
    /// Check if this struct is coming up on the token stream
    fn peek<'s, I>(tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>;
}
