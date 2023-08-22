
use gdml_macros::define_rules;

define_rules! {
    use crate::src::Level;

    enum Op as "operator" {
        Add     -> "+",
        Sub     -> "-",
        Mul     -> "*",
        Div     -> "/",
        Mod     -> "%",

        Seq     -> "=",
        AddSeq  -> "+=",
        SubSeq  -> "-=",
        MulSeq  -> "*=",
        DivSeq  -> "/=",
        ModSeq  -> "%=",

        Eq      -> "==",
        Neq     -> "!=",
        Lss     -> "<",
        Gtr     -> ">",
        Leq     -> "<=",
        Geq     -> ">=",

        And     -> "&&",
        Or      -> "||",

        Not     -> "!",
    }
    
    rule Ident {
        value: String;
        match value:XID_Start & XID_Continue* => {
            Ok(Self {
                value,
                meta: parser.get_meta(start)
            })
        }
    }

    rule ExprList {
        match exprs:(:Expr ";"+)*;
    }

    enum rule AtomExpr =
        ?"if"     -> If |
        ?"let"    -> VarDecl |
        ?"{"      -> Block |
        ?"("      -> ParenExpr |
        ?OP_CHAR  -> UnOp |
                     Float |
        ?'0'..'9' -> Int
        expected "expression";

    enum rule Expr = BinOp | AtomExpr expected "expression";

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
        }
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
        }
    }

    rule ParenExpr {
        match "(" expr:Expr ")";
    }

    rule UnOp {
        match op:Op.Add | Op.Sub | Op.Not target:AtomExpr as Expr;
    }

    rule BinOp {
        match lhs:BinOp[1] as Expr rest:(:Op.* :BinOp[1] as Expr)*;
        match lhs:BinOp[2] as Expr rest:(:Op.Add | Op.Sub :BinOp[2] as Expr)*;
        match lhs:BinOp[3] as Expr rest:(:Op.Mul | Op.Div | Op.Mod :BinOp[3] as Expr)*;
        match lhs:BinOp[4] as Expr rest:(:Op.Eq | Op.Neq :BinOp[4] as Expr)*;
        match lhs:BinOp[5] as Expr rest:(:Op.Leq | Op.Lss | Op.Gtr | Op.Geq :BinOp[5] as Expr)*;
        match lhs:AtomExpr as Expr rest:_;
    }

    rule Block {
        match "{" list:ExprList "}";
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
        match "if" cond:Expr "{" truthy:Expr "}" falsy:(?"else" :If as AtomExpr as Expr);

        impl fn typecheck(&self, state: &mut TypeState) -> Ty {
            state.expect_ty_eq(self.cond.typecheck(), Ty::Bool);
            state.expect_ty_eq(self.truthy.typecheck(), self.falsy.map(|f| f.typecheck()).unwrap_or(Ty::Any))
        }
    }

    enum rule TypeExpr =
        TypeName
        expected "type";

    rule TypeName {
        match ident:Ident;
    }

}
