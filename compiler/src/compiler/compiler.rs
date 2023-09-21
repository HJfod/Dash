
use crate::{
    shared::{src::SrcPool, logging::{Message, LoggerRef}},
    parser::ast::expr::AST
};

pub struct ASTPool<'s> {
    asts: Vec<AST<'s>>,
}

impl<'s> ASTPool<'s> {
    pub fn parse_src_pool(pool: &'s SrcPool, logger: LoggerRef<'s>) -> Result<Self, Message<'s>> {
        Ok(Self {
            asts: pool.iter()
                .map(|src| src.tokenize(logger.clone()).parse())
                .collect::<Result<_, _>>()?
        })
    }

    pub fn iter<'a>(&'s self) -> <&'a Vec<AST<'s>> as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<'s, 'a> IntoIterator for &'a ASTPool<'s> {
    type Item = &'a AST<'s>;
    type IntoIter = <&'a Vec<AST<'s>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.asts.iter()
    }
}
