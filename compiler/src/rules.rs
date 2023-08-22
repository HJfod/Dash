
use gdml_macros::define_rules;

define_rules! {
    use crate::src::Level;

    enum Op {
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
        Lss     -> "<",
        Gtr     -> ">",
        Leq     -> "<=",
        Geq     -> ">=",

        And     -> "&&",
        Or      -> "||",

        Not     -> "!",
    }
    
    rule OpExpr {
        op: Op;
        match v:OP_CHAR+ => {
            let meta = parser.get_meta(start);
            Ok(Self {
                op: v.as_str().try_into().map_err(|_| Message::from_meta(
                    Level::Error, format!("Invalid operator '{v}'"), &meta
                ))?,
                meta
            })
        };
    }

    rule Ident {
        value: String;
        match value:XID_Start & XID_Continue* => {
            Ok(Self {
                value,
                meta: parser.get_meta(start)
            })
        };
    }

    rule ExprList {
        match exprs:(:Expr ";"+)*;
    }

    enum rule AtomExpr =
        ?"if"     -> If |
        ?"let"    -> VarDecl |
        ?"{"      -> Block |
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

    rule UnOp {
        match op:OpExpr target:Expr;
    }

    rule BinOp {
        match lhs:Expr op:OP_CHAR+ rhs:Expr where {
            lhs Op::Mul | Op::Div | Op::Mod rhs;
            lhs Op::Add | Op::Sub rhs;
        };
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
