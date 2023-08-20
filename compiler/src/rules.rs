
use gdml_macros::define_rules;

define_rules! {
    use crate::src::Level;
    
    rule Ident {
        value: String;
        match value:XID_Start & XID_Continue* => {
            Ok(Self {
                value,
                meta: parser.get_meta(start)
            })
        };
    }

    enum rule Expr = If | VarDecl | Int;

    rule Int {
        value: i64;
        match string:'0'..'9'+ => {
            let meta = parser.get_meta(start);
            Ok(Self {
                value: string.parse().map_err(|e| Message::from_meta(
                    Level::Error, format!("Invalid integer: {e}"), &meta
                ))?,
                meta
            })
        };
    }

    rule Float {
        value: f64;
        match string:'0'..'9'+ & "." & '0'..'9'+ => {
            let meta = parser.get_meta(start);
            Ok(Self {
                value: string.parse().map_err(|e| Message::from_meta(
                    Level::Error, format!("Invalid float: {e}"), &meta
                ))?,
                meta
            })
        };
    }

    rule VarDecl {
        match "let" name:Ident ty:(?":" :TypeExpr) value:(?"=" :Expr);

        impl fn typecheck(&self, state: &mut TypeState) -> Ty {
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
        match "if" cond:Expr "{" truthy:Expr "}" falsy:(?"else" "{" :Expr "}");
        match "if" cond:Expr "{" truthy:Expr "}" falsy:(?"else" :If as Expr);

        impl fn typecheck(&self, state: &mut TypeState) -> Ty {
            state.expect_ty_eq(self.cond.typecheck(), Ty::Bool);
            state.expect_ty_eq(self.truthy.typecheck(), self.falsy.map(|f| f.typecheck()).unwrap_or(Ty::Any))
        }
    }

    enum rule TypeExpr = TypeName;

    rule TypeName {
        match ident:Ident;
    }

}
