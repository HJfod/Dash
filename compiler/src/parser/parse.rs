
use std::sync::Arc;
use crate::shared::src::Src;
use super::tokenizer::{TokenIterator, Token};

pub trait Parse: Sized {
    /// Parse this struct from the token stream
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
        where I: Iterator<Item = Token<'s>>;
        
    /// Check if this struct is coming up on the token stream at a position
    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>;
}

macro_rules! impl_tuple_parse {
    ($a: ident; $($r: ident);*) => {
        impl<$a: Parse, $($r: Parse),*> Parse for ($a, $($r),*) {
            fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
                where I: Iterator<Item = Token<'s>>
            {
                Ok(($a::parse(src.clone(), tokenizer)?, $($r::parse(src.clone(), tokenizer)?),*))
            }

            fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
                where I: Iterator<Item = Token<'s>>
            {
                $a::peek(pos, tokenizer)
            }
        }
    };
    ($a: ident) => {
        impl_tuple_parse!($a;);
    };
}

impl_tuple_parse!(A);
impl_tuple_parse!(A; B);
impl_tuple_parse!(A; B; C);
impl_tuple_parse!(A; B; C; D);
impl_tuple_parse!(A; B; C; D; E);

impl<T: Parse> Parse for Option<T> {
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
        where I: Iterator<Item = Token<'s>>
    {
        if Self::peek(0, tokenizer) {
            Ok(Some(T::parse(src, tokenizer)?))
        }
        else {
            Ok(None)
        }
    }

    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        T::peek(pos, tokenizer)
    }
}

/// Marker trait for structs representing single tokens
pub trait IsToken {
    fn is_token() {}
}
