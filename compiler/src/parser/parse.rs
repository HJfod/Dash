
use std::{sync::Arc, marker::PhantomData};
use crate::shared::src::{Src, ArcSpan};
use super::tokenizer::{TokenIterator, Token};

pub fn calculate_span<S: IntoIterator<Item = Option<ArcSpan>>>(spans: S) -> Option<ArcSpan> {
    let mut filtered = spans.into_iter().flatten();
    let mut span = filtered.next()?;
    for ArcSpan(_, range) in filtered {
        if range.start < span.1.start {
            span.1.start = range.start;
        }
        if range.end > span.1.end {
            span.1.end = range.end;
        }
    }
    Some(span.clone())
}

pub trait Parse: Sized {
    /// Parse this type from the token stream
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
        where I: Iterator<Item = Token<'s>>;
        
    /// Check if this type is coming up on the token stream at a position
    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>;
    
    fn span(&self) -> Option<ArcSpan>;

    /// For assertions
    fn is_parseable() {}

    /// If this type is coming up on the token stream based on `Self::peek`, 
    /// then attempt to parse it on the stream
    fn peek_and_parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Option<Self>, ()>
        where I: Iterator<Item = Token<'s>>
    {
        if Self::peek(0, tokenizer) {
            Self::parse(src, tokenizer).map(Some)
        }
        else {
            Ok(None)
        }
    }

    /// Parse a complete token stream into this type, erroring if the whole 
    /// stream couldn't be matched
    fn parse_complete<'s, I, T>(src: Arc<Src>, tokenizer: T) -> Result<Self, ()>
        where
            T: Into<TokenIterator<'s, I>>,
            I: Iterator<Item = Token<'s>>
    {
        let mut stream = tokenizer.into();
        let res = Self::parse(src.clone(), &mut stream)?;
        if stream.peek(0).is_some() {
            stream.expected_eof();
            // This is not a fatal parsing error, so we can continue without 
            // returning Err(())
        }
        Ok(res)
    }
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

            fn span(&self) -> Option<ArcSpan> {
                #[allow(unused_parens, non_snake_case)]
                let ($a $(, $r)*) = &self;
                calculate_span([$a.span() $(, $r.span())*])
            }
        }
    };
    ($a: ident) => {
        impl_tuple_parse!($a;);
    };
}

// impl_tuple_parse!(A);
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

    fn span(&self) -> Option<ArcSpan> {
        self.as_ref().and_then(|s| s.span())
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
        where I: Iterator<Item = Token<'s>>
    {
        let mut res = Vec::new();
        while let Some(t) = T::peek_and_parse(src.clone(), tokenizer)? {
            res.push(t);
        }
        Ok(res)
    }

    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        T::peek(pos, tokenizer)
    }

    fn span(&self) -> Option<ArcSpan> {
        calculate_span(self.iter().map(|s| s.span()))
    }
}

pub struct Separated<T: Parse, S: Parse> {
    items: Vec<T>,
    _phantom: PhantomData<S>,
}

impl<T: Parse, S: Parse> Parse for Separated<T, S> {
    fn parse<'s, I>(src: Arc<Src>, tokenizer: &mut TokenIterator<'s, I>) -> Result<Self, ()>
        where I: Iterator<Item = Token<'s>>
    {
        let mut items = Vec::from([T::parse(src.clone(), tokenizer)?]);
        while S::peek_and_parse(src.clone(), tokenizer)?.is_some() {
            items.push(T::parse(src.clone(), tokenizer)?);
        }
        Ok(Self { items, _phantom: PhantomData })
    }

    fn peek<'s, I>(pos: usize, tokenizer: &TokenIterator<'s, I>) -> bool
        where I: Iterator<Item = Token<'s>>
    {
        T::peek(pos, tokenizer)
    }

    fn span(&self) -> Option<ArcSpan> {
        self.items.span()
    }
}

/// Marker trait for structs representing single tokens
pub trait IsToken: Parse {
    fn is_token() {}
}

impl<T: IsToken> IsToken for Option<T> {}
