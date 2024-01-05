
use crate::{
    parser::parse::{SeparatedWithTrailing, DontExpect, Node, NodePool},
    add_compile_message,
    checker::{resolve::{ResolveNode, ResolveRef}, coherency::{Checker, ScopeID}, ty::Ty, entity::Entity, path},
    shared::{src::ArcSpan, logger::{Message, Level, Note}}
};
use super::{token::{kw, op, punct, delim, Ident}, ty::TypeExpr, expr::{Expr, IdentPath, ExprList}};
use dash_macros::{ParseNode, ResolveNode};

#[derive(Debug, ParseNode)]
pub struct LetDeclNode {
    let_kw: kw::Let,
    name: IdentPath,
    ty: Option<(punct::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
}

impl ResolveNode for LetDeclNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        let ty = self.ty.map(|(_, ty)| ty.resolved_ty(pool)).unwrap_or(Ty::Invalid);
        let value = self.value.map(|(_, ty)| ty.resolved_ty(pool)).unwrap_or(Ty::Invalid);
        let vty = checker.expect_ty_eq(value, ty, self.span(pool));
        let name = self.name.get(pool).to_path(pool);
        match checker.scope().entities_mut().try_push(
            &name,
            Entity::new(
                if self.ty.is_some() || self.value.is_some() {
                    vty
                }
                else {
                    Ty::Undecided(name.to_string(), self.span_or_builtin(pool))
                },
                self.span_or_builtin(pool),
                true
            )
        ) {
            Ok(_) => {}
            Err(old) => {
                let old_span = old.span();
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Item {} has already been defined in this scope", name),
                    self.span_or_builtin(pool).as_ref()
                ).note(Note::new_at("Previous definition here", old_span.as_ref())));
            }
        }
        Some(Ty::Void)
    }
}

// mfw no &'static str in const generics 😢
add_compile_message!(ThisParamMayNotHaveValue: "the 'this' parameter may not have a default value");

#[derive(Debug, ParseNode)]
#[parse(expected = "parameter")]
pub enum FunParamNode {
    NamedParam {
        name: Ident,
        ty: (punct::Colon, TypeExpr),
        default_value: Option<(op::Seq, Expr)>,
    },
    ThisParam {
        this_kw: kw::This,
        ty: Option<(punct::Colon, TypeExpr)>,
        _invalid_value: DontExpect<(op::Seq, Expr), ThisParamMayNotHaveValue>,
    },
}

impl ResolveNode for FunParamNode {
    fn try_resolve_node(&mut self, _: &NodePool, _: &mut Checker) -> Option<Ty> {
        Some(Ty::Invalid)
    }
}

#[derive(Debug, ParseNode)]
pub struct FunDeclNode {
    fun_kw: kw::Fun,
    name: Option<IdentPath>,
    params: delim::Parenthesized<SeparatedWithTrailing<FunParam, punct::Comma>>,
    ret_ty: Option<(punct::Arrow, TypeExpr)>,
    body: delim::Braced<ExprList>,
    #[parse(skip)]
    scope: Option<ScopeID>,
}

impl ResolveNode for FunDeclNode {
    fn try_resolve_node(&mut self, pool: &NodePool, checker: &mut Checker) -> Option<Ty> {
        println!("f0");
        let mut params = Vec::new();
        for param in self.params.get(pool).value.iter() {
            match *param.get(pool) {
                FunParamNode::NamedParam { name, ty, default_value } => {
                    let span = param.get(pool).span(pool);
                    let ty = ty.1.resolved_ty(pool);
                    let v = default_value.map(|(_, ty)| ty.resolved_ty(pool)).unwrap_or(Ty::Invalid);
                    checker.expect_ty_eq(ty.clone(), v, span.clone());
                    params.push((name.get(pool).to_string(), ty, span.unwrap_or(ArcSpan::builtin())));
                }
                FunParamNode::ThisParam { this_kw: _, ty, _invalid_value: _ } => todo!()
            }
        }
        println!("f1");
        let ret_ty = self.ret_ty.map(|(_, ty)| ty.resolved_ty(pool)).unwrap_or(Ty::Invalid);
        let body = {
            let _scope = checker.enter_scope(&mut self.scope);
            for (name, ty, span) in &params {
                if let Err(old) = checker.scope().entities_mut().try_push(
                    &path::IdentPath::new([path::Ident::from(name.as_str())], false),
                    Entity::new(ty.clone(), self.span_or_builtin(pool), true)
                ) {
                    let old_span = old.span();
                    checker.logger().lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Parameter {name} defined multiple times"),
                        span.as_ref()
                    ).note(Note::new_at("Previous definition here", old_span.as_ref())));
                }
            }
            self.body.get(pool).value.resolved_ty(pool)
        };
        checker.expect_ty_eq(body.clone(), ret_ty.clone(), self.body.get(pool).span(pool));

        println!("f2");
        
        let fty = Ty::Function {
            params: params.into_iter().map(|p| (Some(p.0), p.1)).collect(),
            ret_ty: ret_ty.into(),
        };
        if let Some(ref name) = self.name.as_ref().map(|n| n.get(pool).to_path(pool)) {
            if let Err(old) = checker.scope().entities_mut().try_push(
                name,
                Entity::new(fty.clone(), self.span_or_builtin(pool), false)
            ) {
                let old_span = old.span();
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Name {} has already been defined", name),
                    self.span_or_builtin(pool).as_ref()
                ).note(Note::new_at("Previous definition here", old_span.as_ref())));
            }
        }
        Some(fty)
    }
}

#[derive(Debug, ParseNode, ResolveNode)]
#[parse(expected = "item declaration")]
pub enum DeclNode {
    LetDecl(LetDecl),
    FunDecl(FunDecl),
}

