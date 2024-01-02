
use crate::{
    parser::parse::{SeparatedWithTrailing, DontExpect, Parse, calculate_span},
    add_compile_message,
    checker::{resolve::{Resolve, ResolveCache}, coherency::{Checker, ScopeID}, ty::Ty, entity::Entity, path},
    try_resolve,
    shared::{src::ArcSpan, logger::{Message, Level, Note}}
};
use super::{token::{kw, op, punct, delim, Ident}, ty::TypeExpr, expr::{Expr, IdentPath, ExprList}};
use dash_macros::{Parse, Resolve};

#[derive(Debug, Parse)]
pub struct LetDecl {
    let_kw: kw::Let,
    name: IdentPath,
    ty: Option<(punct::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
    #[parse(skip)]
    cache: ResolveCache,
}

impl Resolve for LetDecl {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let ty = try_resolve!(&mut self.ty, checker, Some((_, ty)) => ty);
        let value = try_resolve!(&mut self.value, checker, Some((_, v)) => v);
        let vty = checker.expect_ty_eq(value, ty, self.span());
        match checker.scope().entities().try_push(
            &self.name.to_path(),
            Entity::new(
                if self.ty.is_some() || self.value.is_some() {
                    vty
                }
                else {
                    Ty::Undecided(self.name.to_path().to_string(), self.span_or_builtin())
                },
                self.span_or_builtin(),
                true
            )
        ) {
            Ok(_) => {}
            Err(old) => {
                let old_span = old.span();
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Item {} has already been defined in this scope", self.name.to_path()),
                    self.span_or_builtin().as_ref()
                ).note(Note::new_at("Previous definition here", old_span.as_ref())));
            }
        }
        Some(Ty::Void)
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}

// mfw no &'static str in const generics 😢
add_compile_message!(ThisParamMayNotHaveValue: "the 'this' parameter may not have a default value");

#[derive(Debug, Parse)]
#[parse(expected = "parameter")]
pub enum FunParam {
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

#[derive(Debug, Parse)]
pub struct FunDecl {
    fun_kw: kw::Fun,
    name: Option<IdentPath>,
    params: delim::Parenthesized<SeparatedWithTrailing<FunParam, punct::Comma>>,
    ret_ty: Option<(punct::Arrow, TypeExpr)>,
    body: delim::Braced<ExprList>,
    #[parse(skip)]
    scope: Option<ScopeID>,
    #[parse(skip)]
    cache: ResolveCache,
}

impl Resolve for FunDecl {
    fn try_resolve_impl(&mut self, checker: &mut Checker) -> Option<Ty> {
        let mut params = Vec::new();
        for param in self.params.value.iter_mut() {
            match param {
                FunParam::NamedParam { name, ty, default_value } => {
                    let span = calculate_span([name.span(), ty.span(), default_value.span()]);
                    let ty = ty.1.try_resolve(checker)?;
                    let v = try_resolve!(default_value, checker, Some((_, d)) => d);
                    checker.expect_ty_eq(ty.clone(), v, span.clone());
                    params.push((name.to_string(), ty, span.unwrap_or(ArcSpan::builtin())));
                }
                FunParam::ThisParam { this_kw: _, ty, _invalid_value: _ } => todo!()
            }
        }
        let ret_ty = try_resolve!(&mut self.ret_ty, checker, Some((_, ty)) => ty else Ty::Void);
        let body = {
            let _scope = checker.enter_scope(&mut self.scope);
            for (name, ty, span) in &params {
                if let Err(old) = checker.scope().entities().try_push(
                    &path::IdentPath::new([path::Ident::from(name.as_str())], false),
                    Entity::new(ty.clone(), self.span_or_builtin(), true)
                ) {
                    let old_span = old.span();
                    checker.logger().lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Parameter {name} defined multiple times"),
                        span.as_ref()
                    ).note(Note::new_at("Previous definition here", old_span.as_ref())));
                }
            }
            self.body.value.try_resolve(checker)?
        };
        checker.expect_ty_eq(body.clone(), ret_ty.clone(), self.body.span());
        
        let fty = Ty::Function {
            params: params.into_iter().map(|p| (Some(p.0), p.1)).collect(),
            ret_ty: ret_ty.into(),
        };
        if let Some(ref name) = self.name {
            if let Err(old) = checker.scope().entities().try_push(
                &name.to_path(),
                Entity::new(fty.clone(), self.span_or_builtin(), false)
            ) {
                let old_span = old.span();
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Name {} has already been defined", name.to_path()),
                    self.span_or_builtin().as_ref()
                ).note(Note::new_at("Previous definition here", old_span.as_ref())));
            }
        }
        Some(fty)
    }
    fn cache(&mut self) -> Option<&mut ResolveCache> {
        Some(&mut self.cache)
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "item declaration")]
pub enum Decl {
    LetDecl(Box<LetDecl>),
    FunDecl(Box<FunDecl>),
}

