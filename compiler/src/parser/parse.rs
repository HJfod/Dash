
use std::{sync::Arc, marker::PhantomData, cell::RefCell, rc::Rc};
use crate::{shared::src::{Src, ArcSpan}, checker::{resolve::Resolve, coherency::Checker, ty::Ty}};
use super::tokenizer::TokenIterator;
use as_any::AsAny;

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

pub trait CompileMessage: 'static {
    fn get_msg() -> &'static str;
}

#[macro_export]
macro_rules! add_compile_message {
    ($ident: ident: $msg: literal) => {
        #[derive(Debug)]
        pub struct $ident;
        impl $crate::parser::parse::CompileMessage for $ident {
            fn get_msg() -> &'static str {
                $msg
            }
        }
    };
}

pub trait Node: AsAny {
    /// Get the span of this Node
    fn span(&self) -> Option<ArcSpan>;

    fn span_or_builtin(&self) -> ArcSpan {
        self.span().unwrap_or(ArcSpan::builtin())
    }
}

pub struct FatalParseError;

pub trait ParseFn<'s, T>: FnMut(
    &mut NodeList, Arc<Src>, &mut TokenIterator<'s>
) -> Result<T, FatalParseError> {}

impl<'s, T, F> ParseFn<'s, T> for F
    where F: FnMut(&mut NodeList, Arc<Src>, &mut TokenIterator<'s>) -> Result<T, FatalParseError>
{}

pub trait Parse: Node + Sized {
    /// Parse this type from the token stream
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError>;
        
    /// Check if this type is coming up on the token stream at a position
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool;

    /// If this type is coming up on the token stream based on `Self::peek`, 
    /// then attempt to parse it on the stream
    fn peek_and_parse<'s>(
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator<'s>
    ) -> Result<Option<Self>, FatalParseError> {
        if Self::peek(0, tokenizer) {
            Self::parse(list, src, tokenizer).map(Some)
        }
        else {
            Ok(None)
        }
    }

    /// Parse a complete token stream into this type, erroring if the whole 
    /// stream couldn't be matched
    fn parse_complete<'s, Tk>(
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: Tk
    ) -> Result<Self, FatalParseError>
        where Tk: Into<TokenIterator<'s>>
    {
        let mut stream = tokenizer.into();
        let res = Self::parse(list, src.clone(), &mut stream)?;
        if stream.peek(0).is_some() {
            stream.expected_eof();
            // This is not a fatal parsing error, so we can continue without 
            // returning Err(FatalParseError)
        }
        Ok(res)
    }
}

macro_rules! impl_tuple_parse {
    ($a: ident; $($r: ident);*) => {
        impl<$a: Node, $($r: Node),*> Node for ($a, $($r),*) {
            fn span(&self) -> Option<ArcSpan> {
                #[allow(unused_parens, non_snake_case)]
                let ($a $(, $r)*) = &self;
                calculate_span([$a.span() $(, $r.span())*])
            }
        }

        impl<$a: Parse, $($r: Parse),*> Parse for ($a, $($r),*) {
            fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError>
                where Self: Sized
            {
                Ok(($a::parse(list, src.clone(), tokenizer)?, $($r::parse(list, src.clone(), tokenizer)?),*))
            }

            fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool
                where Self: Sized
            {
                $a::peek(pos, tokenizer)
            }
        }
    };
    ($a: ident) => {
        impl_tuple_parse!($a;);
    };
    (@extract_last $a: ident; $($r: ident;)+) => {
        impl_tuple_parse!(@extract_last $($r;)+)
    };
    (@extract_last $a: ident;) => {
        $a
    };
}

// impl_tuple_parse!(A);
impl_tuple_parse!(A; B);
impl_tuple_parse!(A; B; C);
impl_tuple_parse!(A; B; C; D);
impl_tuple_parse!(A; B; C; D; E);

impl<T: Node> Node for Box<T> {
    fn span(&self) -> Option<ArcSpan> {
        T::span(self)
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        T::parse(list, src, tokenizer).map(Box::from)
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: Node> Node for Option<T> {
    fn span(&self) -> Option<ArcSpan> {
        self.as_ref().and_then(|s| s.span())
    }
}

impl<T: Parse> Parse for Option<T> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        if Self::peek(0, tokenizer) {
            Ok(Some(T::parse(list, src, tokenizer)?))
        }
        else {
            Ok(None)
        }
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: Node> Node for Vec<T> {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span(self.iter().map(|s| s.span()))
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        let mut res = Vec::new();
        while let Some(t) = T::peek_and_parse(list, src.clone(), tokenizer)? {
            res.push(t);
        }
        Ok(res)
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

// todo: Separated and SeparatedWithTrailing could attempt recovery via just 
// consuming tokens until their separator is encountered

#[derive(Debug)]
pub struct OneOrMore<T: Node>(Vec<T>);

impl<T: Node> std::ops::Deref for OneOrMore<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Node> Node for OneOrMore<T> {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span(self.iter().map(|s| s.span()))
    }
}

impl<T: Parse> Parse for OneOrMore<T> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        let mut res = Vec::new();
        res.push(T::parse(list, src.clone(), tokenizer)?);
        while let Some(t) = T::peek_and_parse(list, src.clone(), tokenizer)? {
            res.push(t);
        }
        Ok(OneOrMore(res))
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

#[derive(Debug)]
pub struct Separated<T: Node, S: Node> {
    items: Vec<T>,
    _phantom: PhantomData<S>,
}

impl<T: Node, S: Node> Separated<T, S> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.items.iter_mut()
    }
}

impl<T: Node, S: Node> Node for Separated<T, S> {
    fn span(&self) -> Option<ArcSpan> {
        self.items.span()
    }
}

impl<T: Parse, S: Parse> Parse for Separated<T, S> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        let mut items = Vec::from([T::parse(list, src.clone(), tokenizer)?]);
        while S::peek_and_parse(list, src.clone(), tokenizer)?.is_some() {
            items.push(T::parse(list, src.clone(), tokenizer)?);
        }
        Ok(Self { items, _phantom: PhantomData })
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

#[derive(Debug)]
pub struct SeparatedWithTrailing<T: Node, S: Node> {
    items: Vec<T>,
    trailing: Option<S>,
    _phantom: PhantomData<S>,
}

impl<T: Node, S: Node> SeparatedWithTrailing<T, S> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.items.iter_mut()
    }
}

impl<T: Node, S: Node> Node for SeparatedWithTrailing<T, S> {
    fn span(&self) -> Option<ArcSpan> {
        calculate_span(self.items.iter().map(|i| i.span())
            .chain(self.trailing.as_ref().map(|t| t.span())))
    }
}

impl<T: Parse, S: Parse> Parse for SeparatedWithTrailing<T, S> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        let mut items = Vec::from([T::parse(list, src.clone(), tokenizer)?]);
        let mut trailing = None;
        while let Some(sep) = S::peek_and_parse(list, src.clone(), tokenizer)? {
            if let Some(item) = T::peek_and_parse(list, src.clone(), tokenizer)? {
                items.push(item);
            }
            else {
                trailing = Some(sep);
                break;
            }
        }
        Ok(Self { items, trailing, _phantom: PhantomData })
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

#[derive(Debug)]
pub struct DontExpect<T: Node, M: CompileMessage>(PhantomData<(T, M)>);

impl<T: Node, M: CompileMessage> Node for DontExpect<T, M> {
    fn span(&self) -> Option<ArcSpan> {
        None
    }
}

impl<T: Parse, M: CompileMessage> Parse for DontExpect<T, M> {
    fn parse<'s>(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator<'s>) -> Result<Self, FatalParseError> {
        if T::peek_and_parse(list, src, tokenizer)?.is_some() {
            tokenizer.error(M::get_msg())
        }
        Ok(Self(PhantomData))
    }
    fn peek<'s>(pos: usize, tokenizer: &TokenIterator<'s>) -> bool {
        T::peek(pos, tokenizer)
    }
}

/// Marker trait for structs representing single tokens
pub trait IsToken: Parse {
    fn is_token() {}
}
impl<T: IsToken> IsToken for Option<T> {}
impl<T: IsToken> IsToken for RefToNode<T> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeID(usize);

pub struct NodeRef<T: Node> {
    cell: Rc<RefCell<Box<dyn Node>>>,
    _phantom: PhantomData<T>,
}

#[allow(clippy::should_implement_trait)]
impl<T: Node> NodeRef<T> {
    fn new(cell: Rc<RefCell<Box<dyn Node>>>) -> Self {
        Self { cell, _phantom: PhantomData }
    }
    pub fn as_ref(&self) -> &T {
        self.cell.as_any().downcast_ref().unwrap()
    }
    pub fn as_mut(&mut self) -> &mut T {
        self.cell.as_any_mut().downcast_mut().unwrap()
    }
}

#[derive(Default)]
pub struct NodeList {
    nodes: Vec<Rc<RefCell<Box<dyn Node>>>>,
}

impl NodeList {
    pub fn new() -> Self {
        Self { nodes: vec![] }
    }
    pub fn add<T: Node>(&mut self, t: T) -> RefToNode<T> {
        let t = Rc::from(RefCell::from(Box::from(t) as Box<dyn Node>));
        let id = NodeID(self.nodes.len());
        self.nodes.push(t);
        RefToNode { id, cache: None, _phantom: PhantomData }
    }
    fn get<T: Node>(&self, id: NodeID) -> NodeRef<T> {
        NodeRef::new(self.nodes.get(id.0).unwrap().clone())
    }
}

#[derive(Debug)]
pub struct RefToNode<T: Node> {
    id: NodeID,
    cache: Option<Ty>,
    _phantom: PhantomData<T>,
}

impl<T: Node> RefToNode<T> {
    pub fn get(&self, list: &NodeList) -> NodeRef<T> {
        list.get(self.id)
    }
}

impl<T: Node> Clone for RefToNode<T> {
    fn clone(&self) -> Self {
        Self { id: self.id, cache: self.cache.clone(), _phantom: PhantomData }
    }
}

impl<T: Node> PartialEq for RefToNode<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T: Node> Eq for RefToNode<T> {}

impl<T: Node> Node for RefToNode<T> {
    fn span(&self) -> Option<ArcSpan> {
        todo!()
    }
}

impl<T: Parse> Parse for RefToNode<T> {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let t = T::parse(list, src, tokenizer)?;
        Ok(list.add(t))
    }
    /// Check if this type is coming up on the token stream at a position
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: Resolve> Resolve for RefToNode<T> {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        if let Some(cached) = self.cache.clone() {
            return Some(cached);
        }
        self.cache = list.get::<T>(self.id).as_mut().try_resolve(list, checker);
        self.cache.clone()
    }
}
