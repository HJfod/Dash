
use std::{sync::Arc, marker::PhantomData, cell::RefCell};
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
    /// Get the children of this Node
    fn children(&self, list: &NodeList) -> Vec<NodeID>;

    /// Get the span of this Node
    fn span(&self, list: &NodeList) -> Option<ArcSpan> {
        calculate_span(self.children().into_iter().map(|id| list.get(id).span(list)))
    }

    fn span_or_builtin(&self, list: &NodeList) -> ArcSpan {
        self.span(list).unwrap_or(ArcSpan::builtin())
    }
}

pub trait NodeWithID: 'static {
    fn id(&self) -> NodeID;
}

pub trait ParseWithID: Parse + NodeWithID {}

pub struct FatalParseError;

pub trait ParseFn<'s, T>: FnMut(
    &mut NodeList, Arc<Src>, &mut TokenIterator<'s>
) -> Result<T, FatalParseError> {}

impl<'s, T, F> ParseFn<'s, T> for F
    where F: FnMut(&mut NodeList, Arc<Src>, &mut TokenIterator<'s>) -> Result<T, FatalParseError>
{}

pub trait Parse: Node + Sized {
    /// Parse this type from the token stream
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError>;
        
    /// Check if this type is coming up on the token stream at a position
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool;

    /// If this type is coming up on the token stream based on `Self::peek`, 
    /// then attempt to parse it on the stream
    fn peek_and_parse(
        list: &mut NodeList,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
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
        impl<$a: NodeWithID, $($r: NodeWithID),*> Node for ($a, $($r),*) {
            fn children(&self, _: &NodeList) -> Vec<NodeID> {
                #[allow(unused_parens, non_snake_case)]
                let ($a $(, $r)*) = &self;
                vec![$a.id() $(, $r.id())*]
            }
        }

        impl<$a: ParseWithID, $($r: ParseWithID),*> Parse for ($a, $($r),*) {
            fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError>
                where Self: Sized
            {
                Ok((
                    $a::parse(list, src.clone(), tokenizer)?,
                    $($r::parse(list, src.clone(), tokenizer)?),*
                ))
            }

            fn peek(pos: usize, tokenizer: &TokenIterator) -> bool
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

// impl<T: Node> Node for Box<T> {
//     fn children(&self) -> Vec<NodeID> {
//         self.as_ref().children()
//     }
// }

// impl<T: Parse> Parse for Box<T> {
//     fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
//         T::parse(list, src, tokenizer).map(Box::from)
//     }
//     fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
//         T::peek(pos, tokenizer)
//     }
// }

impl<T: NodeWithID> Node for Option<T> {
    fn children(&self, _: &NodeList) -> Vec<NodeID> {
        self.as_ref().map(|s| vec![s.id()]).unwrap_or_default()
    }
}

impl<T: ParseWithID> Parse for Option<T> {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        if Self::peek(0, tokenizer) {
            Ok(Some(T::parse(list, src, tokenizer)?))
        }
        else {
            Ok(None)
        }
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: NodeWithID> Node for Vec<T> {
    fn children(&self, _: &NodeList) -> Vec<NodeID> {
        self.iter().map(|n| n.id()).collect()
    }
}

impl<T: ParseWithID> Parse for Vec<T> {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let mut res = Vec::new();
        while let Some(t) = T::peek_and_parse(list, src.clone(), tokenizer)? {
            res.push(t);
        }
        Ok(res)
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

// todo: Separated and SeparatedWithTrailing could attempt recovery via just 
// consuming tokens until their separator is encountered

#[derive(Debug)]
pub struct Separated<T: NodeWithID, S: NodeWithID> {
    items: Vec<T>,
    _phantom: PhantomData<S>,
}

impl<T: NodeWithID, S: NodeWithID> Separated<T, S> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.items.iter_mut()
    }
}

impl<T: NodeWithID, S: NodeWithID> Node for Separated<T, S> {
    fn children(&self, list: &NodeList) -> Vec<NodeID> {
        self.items.children(list)
    }
}

impl<T: ParseWithID, S: ParseWithID> Parse for Separated<T, S> {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let mut items = Vec::from([T::parse(list, src.clone(), tokenizer)?]);
        while S::peek_and_parse(list, src.clone(), tokenizer)?.is_some() {
            items.push(T::parse(list, src.clone(), tokenizer)?);
        }
        Ok(Self { items, _phantom: PhantomData })
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

#[derive(Debug)]
pub struct SeparatedWithTrailing<T: NodeWithID, S: NodeWithID> {
    items: Vec<T>,
    trailing: Option<S>,
    _phantom: PhantomData<S>,
}

impl<T: NodeWithID, S: NodeWithID> SeparatedWithTrailing<T, S> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }
    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
        self.items.iter_mut()
    }
}

impl<T: NodeWithID, S: NodeWithID> Node for SeparatedWithTrailing<T, S> {
    fn children(&self, list: &NodeList) -> Vec<NodeID> {
        let mut items = self.items.children(list);
        items.extend(self.trailing.map(|c| c.id()));
        items
    }
}

impl<T: ParseWithID, S: ParseWithID> Parse for SeparatedWithTrailing<T, S> {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
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
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

#[derive(Debug)]
pub struct DontExpect<T: NodeWithID, M: CompileMessage>(PhantomData<(T, M)>);

impl<T: NodeWithID, M: CompileMessage> Node for DontExpect<T, M> {
    fn children(&self, _: &NodeList) -> Vec<NodeID> {
        Default::default()
    }
}

impl<T: ParseWithID, M: CompileMessage> Parse for DontExpect<T, M> {
    fn parse(list: &mut NodeList, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        if T::peek_and_parse(list, src, tokenizer)?.is_some() {
            tokenizer.error(M::get_msg())
        }
        Ok(Self(PhantomData))
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

/// Marker trait for structs representing single tokens
pub trait IsToken {
    fn is_token() {}
}
impl<T: IsToken> IsToken for Option<T> {}
impl<T: IsToken + Node> IsToken for RefToNode<T> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeID(usize);

struct NodeData {
    node: Box<dyn Node>,
    ty: Option<Ty>,
}

#[derive(Default)]
pub struct NodeList {
    nodes: Vec<RefCell<NodeData>>,
}

impl NodeList {
    pub fn new() -> Self {
        Self { nodes: vec![] }
    }
    pub fn add<T: Node>(&mut self, t: T) -> RefToNode<T> {
        let t = RefCell::from(Box::from(t) as Box<dyn Node>);
        let id = NodeID(self.nodes.len());
        self.nodes.push(NodeData { node: t, ty: None });
        RefToNode { id, _phantom: PhantomData }
    }
    fn get(&self, id: NodeID) -> &dyn Node {
        self.nodes.get(id.0).unwrap().borrow().as_ref().node
    }
    fn get_as<T: Node>(&self, id: NodeID) -> &T {
        self.nodes.get(id.0).unwrap().borrow().as_ref().as_any().downcast_ref().unwrap()
    }
}

#[derive(Debug)]
pub struct RefToNode<T: Node> {
    id: NodeID,
    _phantom: PhantomData<T>,
}

impl<T: Node> NodeWithID for RefToNode<T> {
    fn id(&self) -> NodeID {
        self.id
    }
}

impl<T: Node> RefToNode<T> {
    pub fn get(&self, list: &NodeList) -> &T {
        list.get(self.id)
    }
}

impl<T: Node> Clone for RefToNode<T> {
    fn clone(&self) -> Self {
        Self { id: self.id, _phantom: PhantomData }
    }
}
impl<T: Node> Copy for RefToNode<T> {}

impl<T: Node> PartialEq for RefToNode<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T: Node> Eq for RefToNode<T> {}

impl<T: Node> Node for RefToNode<T> {
    fn children(&self, list: &NodeList) -> Vec<NodeID> {
        self.get(list).children(list)
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
