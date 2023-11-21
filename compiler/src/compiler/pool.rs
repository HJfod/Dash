
use crate::{
    shared::{src::SrcPool, logging::LoggerRef},
    parser::ast::expr::AST
};

pub struct ASTPool {
    asts: Vec<AST>,
}

impl ASTPool {
    pub fn parse_src_pool(pool: SrcPool, logger: LoggerRef) -> Self {
        Self {
            asts: pool.iter()
                .map(|src| src.tokenize(logger.clone()).parse())
                .filter_map(|ast| match ast {
                    Ok(ast) => Some(ast),
                    Err(err) => {
                        logger.lock().unwrap().log_msg(err);
                        None
                    }
                })
                .collect()
        }
    }

    pub fn iter<'a>(&'a self) -> <&'a Vec<AST> as IntoIterator>::IntoIter {
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
