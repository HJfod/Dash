
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

    rule Expr {
        enum If, VarDecl, Block, Float, Int, Str, Entity, BinOp, UnOp, Index, Call;

        match :BinOp;
        match[unop] ??OP_CHAR :UnOp;
        match[postfix] :Expr[nonop] afterwards
            while_peek "(" into Call
            while_peek "[" into Index;
        match[nonop] ??"if" :If;
        match ??"let" :VarDecl;
        match ??"{" :Block;
        match ?"(" :Expr ")";
        match ??'"' :Str;
        match :Float;
        match ??'0'..'9' :Int;
        match :Entity;
        expected "expression";
    }

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

    rule Str {
        match '"' value:fn -> String {
            let mut res = String::new();
            while let Ok(c) = parser.expect_not_ch('"') {
                res.push(match c {
                    '\\' => match parser.next().ok_or(parser.error(parser.pos(), "Expected character, found EOF"))? {
                        '\\' => '\\',
                        'n'  => '\n',
                        't'  => '\t',
                        'r'  => '\r',
                        '{'  => '{',
                        '\"' => '\"',
                        '\'' => '\'',
                        '\0' => '\0',
                        c    => Err(parser.error(parser.pos() - 1, format!("Unrecognized escape sequence '\\{c}'")))?,
                    }
                    c => c,
                });
            }
            res
        } '"';
    }

    rule Entity {
        match ident:Ident;
    }

    rule UnOp {
        match op:Op.* target:Expr[unop];
    }

    rule BinOp {
        lhs: Expr<'s>;
        op: Op;
        rhs: Expr<'s>;

        match lhs:BinOp[add] as Expr rest:(:Op.* :BinOp[add] as Expr)* => {
            match Self::reduce(parser, start, lhs, rest)? {
                Expr::BinOp(op) => Ok(*op),
                _ => Err(parser.error(start, "Expected binop")),
            }
        }
        match[add] as Expr lhs:BinOp[mul] as Expr rest:(:Op.Add | Op.Sub :BinOp[mul] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match[mul] as Expr lhs:BinOp[eq] as Expr rest:(:Op.Mul | Op.Div | Op.Mod :BinOp[eq] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match[eq] as Expr lhs:BinOp[comp] as Expr rest:(:Op.Eq | Op.Neq :BinOp[comp] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match[comp] as Expr lhs:BinOp[root] as Expr rest:(:Op.Leq | Op.Lss | Op.Gtr | Op.Geq :BinOp[root] as Expr)* => {
            Self::reduce(parser, start, lhs, rest)
        }
        match[root] as Expr lhs:Expr[unop] => {
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

    rule Index {
        match $expr:Expr "[" index:Expr "]";
    }

    rule Call {
        match $expr:Expr "(" args:(:Expr ~ ("," :Expr) until (")") | ("," ")") ","?) unless ")" ")";
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
        match "if" cond:Expr "{" truthy:Expr "}" falsy:(?"else" :("{" :Expr "}") | If as Expr);

        impl fn typecheck(&self, state: &mut TypeState) -> Ty {
            state.expect_ty_eq(self.cond.typecheck(), Ty::Bool);
            state.expect_ty_eq(self.truthy.typecheck(), self.falsy.map(|f| f.typecheck()).unwrap_or(Ty::Any))
        }
    }

    rule TypeExpr {
        enum TypeName;

        match :TypeName;
    }

    rule TypeName {
        match ident:Ident;
    }

}
