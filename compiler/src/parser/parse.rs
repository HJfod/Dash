
use std::{sync::Arc, marker::PhantomData, cell::RefCell};
use crate::{shared::src::{Src, ArcSpan}, checker::{resolve::{ResolveRef, ResolveNode}, coherency::Checker, ty::Ty, Ice}};
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

pub struct FatalParseError;

// There are two types of AST items: Nodes and Refs
// A Node is an instance of a struct that is owned by a NodePool
// A Node can contain as fields any state as well as Refs to other Nodes, which 
// are considered its children

/// A Node that is allocated on the NodePool
pub trait Node: AsAny {
    /// Get the children of this Node
    fn children(&self) -> Vec<NodeID>;

    /// Get the span of this Node
    fn span(&self, pool: &NodePool) -> Option<ArcSpan> {
        calculate_span(self.children().into_iter().map(|id| pool.get(id).span(pool)))
    }

    fn span_or_builtin(&self, pool: &NodePool) -> ArcSpan {
        self.span(pool).unwrap_or(ArcSpan::builtin())
    }
}

pub trait ParseNode: Node + Sized {
    /// Parse this node and add it to the NodePool, returning its ID in the pool
    fn parse_node(
        pool: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<NodeID, FatalParseError>;

    /// Check if this node is (possibly) coming up on the token stream at a position
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool;
}

/// Reference(s) to a Node in the pool
pub trait Ref: 'static {
    /// Get the ID(s) of the nodes that this Ref is referencing
    fn ids(&self) -> Vec<NodeID>;
}

pub trait ParseRef: Ref + Sized {
    /// Parse this type from the token stream, allocating the node on the 
    /// NodePool and bringing back a reference to it
    fn parse_ref(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError>;
        
    /// Check if this type is coming up on the token stream at a position
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool;

    /// If this type is coming up on the token stream based on `Self::peek`, 
    /// then attempt to parse it on the stream
    fn peek_and_parse(
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: &mut TokenIterator
    ) -> Result<Option<Self>, FatalParseError> {
        if Self::peek(0, tokenizer) {
            Self::parse_ref(list, src, tokenizer).map(Some)
        }
        else {
            Ok(None)
        }
    }

    /// Parse a complete token stream into this type, erroring if the whole 
    /// stream couldn't be matched
    fn parse_complete<'s, Tk>(
        list: &mut NodePool,
        src: Arc<Src>,
        tokenizer: Tk
    ) -> Result<Self, FatalParseError>
        where Tk: Into<TokenIterator<'s>>
    {
        let mut stream = tokenizer.into();
        let res = Self::parse_ref(list, src.clone(), &mut stream)?;
        if stream.peek(0).is_some() {
            stream.expected_eof();
            // This is not a fatal parsing error, so we can continue without 
            // returning Err(FatalParseError)
        }
        Ok(res)
    }
}

pub trait ParseNodeFn: FnMut(
    &mut NodePool, Arc<Src>, &mut TokenIterator
) -> Result<NodeID, FatalParseError> {}

impl<F> ParseNodeFn for F
    where F: FnMut(&mut NodePool, Arc<Src>, &mut TokenIterator) -> Result<NodeID, FatalParseError>
{}

// Ref can be implemented on a bunch of type like Vec<T>, Option<T>, etc.
// However Node should only ever be implemented on a user-defined concrete 
// struct

macro_rules! impl_tuple_parse {
    ($a: ident; $($r: ident);*) => {
        impl<$a: Ref, $($r: Ref),*> Ref for ($a, $($r),*) {
            fn ids(&self) -> Vec<NodeID> {
                #[allow(unused_parens, non_snake_case)]
                let ($a $(, $r)*) = &self;
                $a.ids().into_iter() $(.chain($r.ids()))* .collect()
            }
        }

        impl<$a: ParseRef, $($r: ParseRef),*> ParseRef for ($a, $($r),*) {
            fn parse_ref(
                pool: &mut NodePool,
                src: Arc<Src>,
                tokenizer: &mut TokenIterator
            ) -> Result<Self, FatalParseError> {
                Ok((
                    $a::parse_ref(pool, src.clone(), tokenizer)?,
                    $($r::parse_ref(pool, src.clone(), tokenizer)?),*
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

impl<T: Ref> Ref for Option<T> {
    fn ids(&self) -> Vec<NodeID> {
        self.as_ref().map(|s| s.ids()).unwrap_or_default()
    }
}

impl<T: ParseRef> ParseRef for Option<T> {
    fn parse_ref(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        if Self::peek(0, tokenizer) {
            Ok(Some(T::parse_ref(pool, src, tokenizer)?))
        }
        else {
            Ok(None)
        }
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: Ref> Ref for Vec<T> {
    fn ids(&self) -> Vec<NodeID> {
        self.iter().flat_map(|n| n.ids()).collect()
    }
}

impl<T: ParseRef> ParseRef for Vec<T> {
    fn parse_ref(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let mut res = Vec::new();
        while let Some(t) = T::peek_and_parse(pool, src.clone(), tokenizer)? {
            res.push(t);
        }
        Ok(res)
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: ResolveRef> ResolveRef for Vec<T> {
    fn try_resolve_ref(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let mut some_unresolved = false;
        for item in self.iter_mut() {
            if item.try_resolve_ref(pool, checker).is_none() {
                some_unresolved = true;
            }
        }
        (!some_unresolved).then_some(Ty::Invalid)
    }
    fn resolved_ty(&self, _: &NodePool) -> Ty {
        Ty::Invalid
    }
}

// todo: Separated and SeparatedWithTrailing could attempt recovery via just 
// consuming tokens until their separator is encountered

#[derive(Debug)]
pub struct Separated<T: Ref, S: Ref> {
    items: Vec<T>,
    _phantom: PhantomData<S>,
}

impl<T: Ref, S: Ref> Separated<T, S> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }
    // pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
    //     self.items.iter_mut()
    // }
}

impl<T: Ref, S: Ref> Ref for Separated<T, S> {
    fn ids(&self) -> Vec<NodeID> {
        self.items.ids()
    }
}

impl<T: ParseRef, S: ParseRef> ParseRef for Separated<T, S> {
    fn parse_ref(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let mut items = Vec::from([T::parse_ref(pool, src.clone(), tokenizer)?]);
        while S::peek_and_parse(pool, src.clone(), tokenizer)?.is_some() {
            items.push(T::parse_ref(pool, src.clone(), tokenizer)?);
        }
        Ok(Self { items, _phantom: PhantomData })
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: ResolveRef, S: Ref> ResolveRef for Separated<T, S> {
    fn try_resolve_ref(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        self.items.try_resolve_ref(pool, checker)
    }
    fn resolved_ty(&self, _: &NodePool) -> Ty {
        Ty::Invalid
    }
}

#[derive(Debug)]
pub struct SeparatedWithTrailing<T: Ref, S: Ref> {
    items: Vec<T>,
    trailing: Option<S>,
    _phantom: PhantomData<S>,
}

impl<T: Ref, S: Ref> SeparatedWithTrailing<T, S> {
    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.items.iter()
    }
    // pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, T> {
    //     self.items.iter_mut()
    // }
}

impl<T: Ref, S: Ref> Ref for SeparatedWithTrailing<T, S> {
    fn ids(&self) -> Vec<NodeID> {
        self.items.ids().into_iter()
            .chain(self.trailing.as_ref().map(|c| c.ids()).unwrap_or_default())
            .collect()
    }
}

impl<T: ParseRef, S: ParseRef> ParseRef for SeparatedWithTrailing<T, S> {
    fn parse_ref(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        let mut items = Vec::from([T::parse_ref(pool, src.clone(), tokenizer)?]);
        let mut trailing = None;
        while let Some(sep) = S::peek_and_parse(pool, src.clone(), tokenizer)? {
            if let Some(item) = T::peek_and_parse(pool, src.clone(), tokenizer)? {
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

impl<T: ResolveRef + ParseRef, S: Ref> ResolveRef for SeparatedWithTrailing<T, S> {
    fn try_resolve_ref(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        self.items.try_resolve_ref(pool, checker)
    }
    fn resolved_ty(&self, _: &NodePool) -> Ty {
        Ty::Invalid
    }
}

#[derive(Debug)]
pub struct DontExpect<T: Ref, M: CompileMessage>(PhantomData<(T, M)>);

impl<T: Ref, M: CompileMessage> Ref for DontExpect<T, M> {
    fn ids(&self) -> Vec<NodeID> {
        Default::default()
    }
}

impl<T: ParseRef, M: CompileMessage> ParseRef for DontExpect<T, M> {
    fn parse_ref(list: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        if T::peek_and_parse(list, src, tokenizer)?.is_some() {
            tokenizer.error(M::get_msg())
        }
        Ok(Self(PhantomData))
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: ParseRef, M: CompileMessage> ResolveRef for DontExpect<T, M> {
    fn try_resolve_ref(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
    fn resolved_ty(&self, _: &NodePool) -> Ty {
        Ty::Invalid
    }
}

/// Marker trait for structs representing single tokens
pub trait IsToken {
    fn assert_ty_is_token() {}
}
impl<T: IsToken> IsToken for Option<T> {}
impl<T: IsToken + ResolveNode> IsToken for RefToNode<T> {}

/// An unique ID for a node in the NodePool
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeID(usize);

struct NodeData {
    node: Box<dyn ResolveNode>,
    ty: Option<Ty>,
}

/// Pool containing all allocated Nodes. There should only be one pool for each 
/// codebase in compilation, and all of that codebase's source files should 
/// share the same pool - this way we can conserve memory and do some funky 
/// optimizations later on (such as interning)
#[derive(Default)]
pub struct NodePool {
    nodes: Vec<RefCell<NodeData>>,
}

impl NodePool {
    /// Create a new empty pool
    pub fn new() -> Self {
        Self { nodes: vec![] }
    }
    /// Add a new Node to this pool. Returns the added node's ID
    pub fn add<N: ResolveNode>(&mut self, t: N) -> NodeID {
        let t = RefCell::from(NodeData {
            node: Box::from(t) as Box<dyn ResolveNode>,
            ty: None,
        });
        let id = NodeID(self.nodes.len());
        self.nodes.push(t);
        id
    }
    fn get(&self, id: NodeID) -> std::cell::Ref<'_, dyn ResolveNode> {
        std::cell::Ref::map(
            self.nodes.get(id.0).unwrap().borrow(),
            |e| e.node.as_ref()
        )
    }
    fn get_as<T: Node>(&self, id: NodeID) -> std::cell::Ref<'_, T> {
        std::cell::Ref::map(
            self.nodes.get(id.0).unwrap().borrow(),
            |e| e.node.as_ref().as_any().downcast_ref().unwrap()
        )
    }
    fn get_ty(&self, id: NodeID) -> Option<Ty> {
        self.nodes.get(id.0).unwrap().borrow().ty.clone()
    }
    fn get_mut(&self, id: NodeID) -> std::cell::RefMut<'_, dyn ResolveNode> {
        std::cell::RefMut::map(
            self.nodes.get(id.0).unwrap().borrow_mut(),
            |e| e.node.as_mut()
        )
    }
    fn get_as_mut<T: ResolveNode>(&self, id: NodeID) -> std::cell::RefMut<'_, T> {
        std::cell::RefMut::map(
            self.nodes.get(id.0).unwrap().borrow_mut(),
            |e| e.node.as_mut().as_any_mut().downcast_mut().unwrap()
        )
    }
    fn get_ty_mut(&self, id: NodeID) -> std::cell::RefMut<'_, Option<Ty>> {
        std::cell::RefMut::map(
            self.nodes.get(id.0).unwrap().borrow_mut(),
            |e| &mut e.ty
        )
    }
}

/// A strongly-typed reference to a Node in the pool
#[derive(Debug)]
pub struct RefToNode<T: ResolveNode>(NodeID, PhantomData<T>);

impl<T: ResolveNode> RefToNode<T> {
    /// You better know for sure that the ID of Node is the correct one >:(
    pub fn new_raw(id: NodeID) -> Self {
        Self(id, PhantomData)
    }
    pub fn new(pool: &mut NodePool, item: T) -> Self {
        Self(pool.add(item), PhantomData)
    }
    pub fn get<'a>(&self, pool: &'a NodePool) -> std::cell::Ref<'a, T> {
        pool.get_as(self.0)
    }
}

impl<T: ResolveNode> Clone for RefToNode<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T: ResolveNode> Copy for RefToNode<T> {}

impl<T: ResolveNode> PartialEq for RefToNode<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T: ResolveNode> Eq for RefToNode<T> {}

impl<T: ResolveNode> Ref for RefToNode<T> {
    fn ids(&self) -> Vec<NodeID> {
        vec![self.0]
    }
}

impl<T: ResolveNode + ParseNode> ParseRef for RefToNode<T> {
    fn parse_ref(list: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<Self, FatalParseError> {
        Ok(Self(T::parse_node(list, src, tokenizer)?, PhantomData))
    }
    fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
        T::peek(pos, tokenizer)
    }
}

impl<T: ResolveNode> ResolveRef for RefToNode<T> {
    fn try_resolve_ref(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        if let Some(ty) = pool.get_ty(self.0) {
            return Some(ty);
        }
        let mut some_unresolved = false;
        for child in self.get(pool).children() {
            if pool.get_mut(child).try_resolve_node(pool, checker).is_none() {
                some_unresolved = true;
            }
        }
        if some_unresolved {
            return None;
        }
        let ty = pool.get_as_mut::<T>(self.0).try_resolve_node(pool, checker)?;
        *pool.get_ty_mut(self.0) = Some(ty.clone());
        Some(ty)
    }
    fn resolved_ty(&self, pool: &NodePool) -> Ty {
        pool.get_ty(self.0).ice("resolved_ty called on an unresolved node")
    }
}
