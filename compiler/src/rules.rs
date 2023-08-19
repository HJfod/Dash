
define_rules! {

    rule Ident -> String {
        match :join(a:XID_Start b:join(:XID_Continue)*)
    }

    rule Expr {
        match value:enum(If, VarDecl);

        impl typecheck(&self, state: &mut TypeState) -> Ty {
            match self.value {
                If(stmt) => stmt.typecheck(),
                VarDecl(decl) => decl.typecheck(),
            }
        }
    }

    rule VarDecl {
        match "let" name:Ident ty:(":" :TypeExpr)? value:("=" :Expr)?;

        impl typecheck(&self, state: &mut TypeState) -> Ty {
            state.push_entity(Var::new(self.name, match (self.ty, self.value) {
                (Some(t), Some(v)) => state.expect_ty_eq(t, v),
                (Some(t), None)    => t,
                (None,    Some(v)) => v,
                (None,    None)    => {
                    state.error("Variables need an explicit type or value to infer it from");
                    Ty::Invalid
                },
            }));
            Ty::Void
        }
    }

    rule If {
        match "if" cond:Expr "{" truthy:Expr "}" falsy:("else" "{" :Expr "}")?;
        match "if" cond:Expr "{" truthy:Expr "}" falsy:("else" :If)?;

        impl typecheck(&self, state: &mut TypeState) -> Ty {
            state.expect_ty_eq(self.cond.typecheck(), Ty::Bool);
            state.expect_ty_eq(self.truthy.typecheck(), self.falsy.map(|f| f.typecheck()).unwrap_or(Ty::Any))
        }
    }

    rule TypeExpr {}

    rule TypeName {
        match Ident;
    }

}
