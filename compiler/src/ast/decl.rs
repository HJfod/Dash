
use crate::{
    parser::parse::{SeparatedWithTrailing, DontExpect, Parse, calculate_span, Node, NodeList},
    add_compile_message,
    checker::{resolve::Resolve, coherency::{Checker, ScopeID}, ty::Ty, entity::Entity, path},
    try_resolve,
    shared::{src::ArcSpan, logger::{Message, Level, Note}}
};
use super::{token::{kw, op, punct, delim, Ident}, ty::TypeExpr, expr::{Expr, IdentPath, ExprList}};
use dash_macros::{Parse, Resolve};

#[derive(Debug, Parse)]
pub struct LetDeclItem {
    let_kw: kw::Let,
    name: IdentPath,
    ty: Option<(punct::Colon, TypeExpr)>,
    value: Option<(op::Seq, Expr)>,
}

impl Resolve for LetDeclItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        let ty = try_resolve!(&mut self.ty, list, checker, Some((_, ty)) => ty);
        let value = try_resolve!(&mut self.value, list, checker, Some((_, v)) => v);
        let vty = checker.expect_ty_eq(value, ty, self.span(list));
        let name = self.name.get(list).as_ref().to_path(list);
        match checker.scope().entities_mut().try_push(
            &name,
            Entity::new(
                if self.ty.is_some() || self.value.is_some() {
                    vty
                }
                else {
                    Ty::Undecided(name.to_string(), self.span_or_builtin(list))
                },
                self.span_or_builtin(list),
                true
            )
        ) {
            Ok(_) => {}
            Err(old) => {
                let old_span = old.span();
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Item {} has already been defined in this scope", name),
                    self.span_or_builtin(list).as_ref()
                ).note(Note::new_at("Previous definition here", old_span.as_ref())));
            }
        }
        Some(Ty::Void)
    }
}

// mfw no &'static str in const generics 😢
add_compile_message!(ThisParamMayNotHaveValue: "the 'this' parameter may not have a default value");

#[derive(Debug, Parse)]
#[parse(expected = "parameter")]
pub enum FunParamItem {
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
pub struct FunDeclItem {
    fun_kw: kw::Fun,
    name: Option<IdentPath>,
    params: delim::Parenthesized<SeparatedWithTrailing<FunParam, punct::Comma>>,
    ret_ty: Option<(punct::Arrow, TypeExpr)>,
    body: delim::Braced<ExprList>,
    #[parse(skip)]
    scope: Option<ScopeID>,
}

impl Resolve for FunDeclItem {
    fn try_resolve(&mut self, list: &mut NodeList, checker: &mut Checker) -> Option<Ty> {
        println!("f0");
        let mut params = Vec::new();
        for param in self.params.get(list).as_mut().value.iter_mut() {
            match param.get(list).as_mut() {
                FunParamItem::NamedParam { name, ty, default_value } => {
                    let span = calculate_span([name.span(list), ty.span(list), default_value.span(list)]);
                    let ty = ty.1.try_resolve(list, checker)?;
                    let v = try_resolve!(default_value, list, checker, Some((_, d)) => d);
                    checker.expect_ty_eq(ty.clone(), v, span.clone());
                    params.push((name.get(list).as_ref().to_string(), ty, span.unwrap_or(ArcSpan::builtin())));
                }
                FunParamItem::ThisParam { this_kw: _, ty, _invalid_value: _ } => todo!()
            }
        }
        println!("f1");
        let ret_ty = try_resolve!(&mut self.ret_ty, list, checker, Some((_, ty)) => ty else Ty::Void);
        let body = {
            let _scope = checker.enter_scope(&mut self.scope);
            for (name, ty, span) in &params {
                if let Err(old) = checker.scope().entities_mut().try_push(
                    &path::IdentPath::new([path::Ident::from(name.as_str())], false),
                    Entity::new(ty.clone(), self.span_or_builtin(list), true)
                ) {
                    let old_span = old.span();
                    checker.logger().lock().unwrap().log(Message::new(
                        Level::Error,
                        format!("Parameter {name} defined multiple times"),
                        span.as_ref()
                    ).note(Note::new_at("Previous definition here", old_span.as_ref())));
                }
            }
            self.body.get(list).as_mut().value.try_resolve(list, checker)?
        };
        checker.expect_ty_eq(body.clone(), ret_ty.clone(), self.body.span(list));

        println!("f2");
        
        let fty = Ty::Function {
            params: params.into_iter().map(|p| (Some(p.0), p.1)).collect(),
            ret_ty: ret_ty.into(),
        };
        if let Some(ref name) = self.name.as_ref().map(|n| n.get(list).as_ref().to_path(list)) {
            if let Err(old) = checker.scope().entities_mut().try_push(
                name,
                Entity::new(fty.clone(), self.span_or_builtin(list), false)
            ) {
                let old_span = old.span();
                checker.logger().lock().unwrap().log(Message::new(
                    Level::Error,
                    format!("Name {} has already been defined", name),
                    self.span_or_builtin(list).as_ref()
                ).note(Note::new_at("Previous definition here", old_span.as_ref())));
            }
        }
        Some(fty)
    }
}

#[derive(Debug, Parse, Resolve)]
#[parse(expected = "item declaration")]
pub enum DeclItem {
    LetDecl(Box<LetDecl>),
    FunDecl(Box<FunDecl>),
}

