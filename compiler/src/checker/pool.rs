
use crate::ast::expr::ExprList;
use crate::parser::parse::Parse;
use crate::parser::tokenizer::Tokenizer;
use crate::shared::src::SrcPool;
use crate::shared::logger::LoggerRef;

pub type AST = ExprList;

pub struct ASTPool {
    asts: Vec<AST>,
}

impl<'s: 'g, 'g> ASTPool {
    pub fn parse_src_pool(pool: &SrcPool, logger: LoggerRef) -> Self {
        Self {
            asts: pool.iter()
                .filter_map(|src| ExprList::parse_complete(
                    src.clone(),
                    Tokenizer::new(&src, logger.clone())
                ).ok())
                .collect()
        }
    }
    pub fn iter(&self) -> <&Vec<AST> as IntoIterator>::IntoIter {
        self.into_iter()
    }
}

impl<'a> IntoIterator for &'a ASTPool {
    type Item = &'a AST;
    type IntoIter = <&'a Vec<AST> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.asts.iter()
    }
}

impl<'a> IntoIterator for &'a mut ASTPool {
    type Item = &'a mut AST;
    type IntoIter = <&'a mut Vec<AST> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.asts.iter_mut()
    }
}