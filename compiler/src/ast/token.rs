
use std::fmt::Display;

use dash_macros::token;

#[token(kind = "Ident", include_raw)]
pub struct Ident {}

impl Display for IdentNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.raw)
    }
}

pub(crate) mod kw {
    use dash_macros::token;

    #[token(kind = "Keyword", raw = "let")]
    pub struct Let {}
    #[token(kind = "Keyword", raw = "fun")]
    pub struct Fun {}
    #[token(kind = "Keyword", raw = "if")]
    pub struct If {}
    #[token(kind = "Keyword", raw = "else")]
    pub struct Else {}
    #[token(kind = "Keyword", raw = "this")]
    pub struct This {}
    #[token(kind = "Keyword", raw = "return")]
    pub struct Return {}
    #[token(kind = "Keyword", raw = "using")]
    pub struct Using {}

    #[token(kind = "Ident", raw = "get")]
    pub struct Get {}
    #[token(kind = "Ident", raw = "set")]
    pub struct Set {}
}

pub(crate) mod lit {
    use dash_macros::{token, ParseNode};

    use crate::{checker::{resolve::ResolveNode, coherency::Checker, ty::Ty}, parser::parse::NodePool};

    #[token(kind = "Keyword", raw = "void", no_default_resolve)]
    pub struct Void {}

    impl ResolveNode for VoidNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Void)
        }
    }

    #[token(kind = "Keyword", raw = "true")]
    pub struct True {}

    #[token(kind = "Keyword", raw = "false")]
    pub struct False {}

    #[derive(Debug, ParseNode)]
    #[parse(expected = "boolean")]
    pub enum BoolNode {
        True(True),
        False(False),
    }

    impl ResolveNode for BoolNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Bool)
        }
    }

    #[token(kind = "Int(_)", no_default_resolve)]
    pub struct Int {
        value: i64,
    }

    impl ResolveNode for IntNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Int)
        }
    }

    #[token(kind = "Float(_)", no_default_resolve)]
    pub struct Float {
        value: f64,
    }

    impl ResolveNode for FloatNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Float)
        }
    }

    #[token(kind = "String(_)", no_default_resolve)]
    pub struct String {
        value: std::string::String,
    }

    impl ResolveNode for StringNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::String)
        }
    }
}

pub(crate) mod punct {
    use std::sync::Arc;

    use dash_macros::token;

    use crate::{
        shared::{src::Src, logger::{Message, Level}},
        parser::{
            parse::{ParseNode, FatalParseError, calculate_span, NodePool, Node, NodeID, ParseRef, RefToNode},
            tokenizer::TokenIterator
        }, checker::{resolve::{ResolveNode, ResolveRef}, coherency::Checker, ty::Ty}
    };

    #[token(kind = "Punct", raw = ",")]
    pub struct Comma {}

    #[token(kind = "Punct", raw = ";")]
    pub struct Semicolon {}

    #[derive(Debug)]
    pub struct TerminatingSemicolonNode {
        semicolons: Vec<Semicolon>,
    }
    pub type TerminatingSemicolon = RefToNode<TerminatingSemicolonNode>;

    impl TerminatingSemicolonNode {
        pub fn has_semicolon(&self) -> bool {
            !self.semicolons.is_empty()
        }
    }

    impl Node for TerminatingSemicolonNode {
        fn children(&self) -> Vec<&dyn ResolveRef> {
            vec![&self.semicolons]
        }
    }

    impl ParseNode for TerminatingSemicolonNode {
        fn parse_node(pool: &mut NodePool, src: Arc<Src>, tokenizer: &mut TokenIterator) -> Result<NodeID, FatalParseError> {
            let last_was_braced = tokenizer.last_was_braced();
            let mut found = vec![];
            while let Some(s) = Semicolon::peek_and_parse(pool, src.clone(), tokenizer)? {
                found.push(s);
            }
            // If the last token was a Braced or we're at EOF of this tree 
            // then allow omitting semicolon
            if found.is_empty() && !last_was_braced && tokenizer.peek(0).is_some() {
                tokenizer.expected("semicolon");
            }
            // Warn if there were multiple semicolons
            if !found.is_empty() && last_was_braced {
                tokenizer.logger().lock().unwrap().log(Message::new(
                    Level::Warning,
                    if found.len() > 1 {
                        "Unnecessary semicolons"
                    }
                    else {
                        "Unnecessary semicolon"
                    },
                    calculate_span(found.iter().map(|s| s.get(pool).span(pool))).unwrap().as_ref()
                ));
            }
            else if found.len() > 1 {
                tokenizer.logger().lock().unwrap().log(Message::new(
                    Level::Warning,
                    "Unnecessary semicolons",
                    calculate_span(found.iter().skip(1).map(|s| s.get(pool).span(pool))).unwrap().as_ref()
                ));
            }
            // Missing semicolon is not a fatal parsing error
            Ok(pool.add(Self { semicolons: found }))
        }
        fn peek(pos: usize, tokenizer: &TokenIterator) -> bool {
            Semicolon::peek(pos, tokenizer)
        }
    }

    impl ResolveNode for TerminatingSemicolonNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Invalid)
        }
    }

    #[token(kind = "Punct", raw = ":")]
    pub struct Colon {}

    #[token(kind = "Punct", raw = "::")]
    pub struct Namespace {}

    #[token(kind = "Punct", raw = "->")]
    pub struct Arrow {}

    #[token(kind = "Punct", raw = "=>")]
    pub struct FatArrow {}

    #[token(kind = "Punct", raw = "@")]
    pub struct At {}
}

pub(crate) mod op {
    use std::fmt::Display;
    use std::hash::Hash;

    use dash_macros::{token, ParseNode};
    use crate::parser::parse::ParseRef;
    use crate::parser::tokenizer::TokenIterator;
    use crate::checker::resolve::ResolveNode;
    use crate::NodePool;
    use crate::Checker;
    use crate::Ty;

    macro_rules! declare_ops {
        ($group: ident {
            $($name: ident = $raw: literal),*
            $(,)?
        }) => {
            $(
                #[token(kind = "Punct", raw = $raw)]
                pub struct $name {}
            )*

            concat_idents::concat_idents!(op_name = $group, Op {
                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                pub enum op_name {
                    $($name),*
                }

                impl Display for op_name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        match self {
                            $(Self::$name => f.write_str($raw)),*
                        }
                    }
                }

                $(
                    impl $name {
                        pub fn op(&self) -> op_name {
                            op_name::$name
                        }
                    }
                )*
            });

            concat_idents::concat_idents!(item_name = $group, Node {
                #[derive(Debug, ParseNode)]
                #[parse(expected = "operator")]
                pub enum item_name {
                    $($name($name)),*
                }
                impl ResolveNode for item_name {
                    fn try_resolve_node(
                        &mut self,
                        _: &NodePool,
                        _: &mut Checker
                    ) -> Option<Ty> {
                        Some(Ty::Invalid)
                    }
                }
                impl item_name {
                    concat_idents::concat_idents!(op_name = $group, Op {
                        pub fn op(&self) -> op_name {
                            match self {
                                $(Self::$name(a) => a.op()),*
                            }
                        }
                    });
                }
            });
        };
    }

    declare_ops! {
        Unary {
            Not = "!",
            Question = "?",
            Plus = "+",
            Neg = "-",
        }
    }

    declare_ops! {
        Binary {
            Eq = "==", Neq = "!=",
            And = "&&", Or = "||",
            Seq = "=",
            Add = "+", Sub = "-",
            Mul = "*", Div = "/", Mod = "%",
            Grt = ">", Geq = ">=", Less = "<", Leq = "<=",
        }
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub enum Prec {
        Mul,
        Add,
        Ord,
        Eq,
        And,
        Or,
        Seq,
    }

    impl Prec {
        pub(crate) const fn order() -> [Prec; 7] {
            [Prec::Mul, Prec::Add, Prec::Ord, Prec::Eq, Prec::And, Prec::Or, Prec::Seq]
        }
        pub fn peek(&self, tokenizer: &TokenIterator) -> bool {
            match self {
                Prec::Mul => Mul::peek(0, tokenizer) || Div::peek(0, tokenizer) ||
                             Mod::peek(0, tokenizer),
                Prec::Ord => Grt::peek(0, tokenizer) || Less::peek(0, tokenizer) ||
                             Geq::peek(0, tokenizer) || Leq::peek(0, tokenizer),
                Prec::Add => Add::peek(0, tokenizer) || Sub::peek(0, tokenizer),
                Prec::Eq  => Eq::peek(0, tokenizer) || Neq::peek(0, tokenizer),
                Prec::And => And::peek(0, tokenizer),
                Prec::Or  => Or::peek(0, tokenizer),
                Prec::Seq => Seq::peek(0, tokenizer),
            }
        }
    }
}

pub(crate) mod delim {
    use dash_macros::{token, ParseNode};

    use crate::{
        parser::parse::{NodePool, ParseRef},
        checker::{resolve::{ResolveNode, ResolveRef}, coherency::Checker, ty::Ty}
    };

    #[token(kind = "Parentheses(_)", value_is_token_tree, no_default_resolve)]
    pub struct Parenthesized<T: ParseRef + ResolveRef> {
        pub value: T,
    }

    impl<T: ResolveRef + ParseRef> ResolveNode for ParenthesizedNode<T> {
        fn try_resolve_node(&mut self, pool: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(self.value.resolved_ty(pool))
        }
    }
     
    #[token(kind = "Brackets(_)", value_is_token_tree, no_default_resolve)]
    pub struct Bracketed<T: ParseRef + ResolveRef> {
        pub value: T,
    }

    impl<T: ResolveRef + ParseRef> ResolveNode for BracketedNode<T> {
        fn try_resolve_node(&mut self, pool: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(self.value.resolved_ty(pool))
        }
    }

    #[token(kind = "Braces(_)", value_is_token_tree, no_default_resolve)]
    pub struct Braced<T: ParseRef + ResolveRef> {
        pub value: T,
    }

    impl<T: ResolveRef + ParseRef> ResolveNode for BracedNode<T> {
        fn try_resolve_node(&mut self, pool: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(self.value.resolved_ty(pool))
        }
    }

    /// Placeholder used for peeking delimiters
    #[derive(Debug, ParseNode)]
    pub struct PNode;

    impl ResolveNode for PNode {
        fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
            Some(Ty::Invalid)
        }
    }
}
