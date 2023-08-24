
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
        match exprs:(:Expr ";"+) until "}" | EOF;
    }

    enum rule AtomExpr =
        ?"if"     -> If |
        ?"let"    -> VarDecl |
        ?"{"      -> Block |
        ?"("      -> ParenExpr |
                     Float |
        ?'0'..'9' -> Int |
                     Entity
        expected "expression";
    
    enum rule Expr =
        BinOp |
        UnOp |
        CallExpr |
        AtomExpr
        expected "expression";

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

    rule Entity {
        match ident:Ident;
    }

    rule ParenExpr {
        match "(" expr:Expr ")";
    }

    rule UnOp {
        match op:Op.Add | Op.Sub | Op.Not target:CallExpr as Expr | AtomExpr as Expr;
    }

    rule BinOp {
        lhs: Expr<'s>;
        op: Op;
        rhs: Expr<'s>;

        match lhs:BinOp[1] as Expr rest:(:Op.* :BinOp[1] as Expr)* => {
            match Self::reduce(parser, start, lhs, rest)? {
                Expr::BinOp(op) => Ok(*op),
                _ => Err(parser.error(start, "Expected binop")),
            }
        }
        match as Expr lhs:BinOp[2] as Expr rest:(:Op.Add | Op.Sub :BinOp[2] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match as Expr lhs:BinOp[3] as Expr rest:(:Op.Mul | Op.Div | Op.Mod :BinOp[3] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match as Expr lhs:BinOp[4] as Expr rest:(:Op.Eq | Op.Neq :BinOp[4] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match as Expr lhs:BinOp[5] as Expr rest:(:Op.Leq | Op.Lss | Op.Gtr | Op.Geq :BinOp[5] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match as Expr lhs:UnOp as Expr | CallExpr as Expr | AtomExpr as Expr => {
            Ok(lhs)
        }

        fn reduce(parser: &mut Parser<'s>, start: usize, lhs: Expr<'s>, rest: Vec<(Op, Expr<'s>)>) -> Result<Expr<'s>, Message<'s>> {
            Ok(rest.into_iter().fold(lhs, |mem, curr| {
                Expr::from(Self {
                    lhs: mem,
                    op: curr.0,
                    rhs: curr.1,
                    meta: parser.get_meta(start),
                })
            }))
        }
    }

    rule CallExpr {
        match expr:AtomExpr "(" args:(:Expr ~ ("," :Expr)* ","?)? ")";
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
