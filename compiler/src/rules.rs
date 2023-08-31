use gdml_macros::define_rules;

define_rules! {
    use crate::src::Level;
    use crate::compiler;

    keywords {
        "let", "fun", "decl", "struct",
        "is", "as",
        "if", "else", "for", "while",
        "extern", "export", "import",
        reserve "match",
        reserve "switch",
    }

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
            if is_keyword(&value) {
                Err(parser.error(start, format!("Expected identifier, got keyword '{value}'")))
            }
            else if is_reserved_keyword(&value) {
                Err(parser.error(start, format!("Expected identifier, got reserved keyword '{value}'")))
            }
            else {
                Ok(Self {
                    value,
                    meta: parser.get_meta(start)
                })
            }
        }

        typecheck {
            yield Ty::Never;
        }

        fn path(&self) -> compiler::Path {
            compiler::Path::new([self.value.clone()], false)
        }
    }

    rule Path {
        match absolute:"::"? components:Ident ~ ("::" :Ident)*;

        typecheck {
            yield Ty::Never;
        }

        fn path(&self) -> compiler::Path {
            compiler::Path::new(
                self.components.iter().map(|c| c.value.clone()).collect::<Vec<_>>(),
                self.absolute.is_some()
            )
        }
    }

    rule ExprList {
        match exprs:(:Expr ";"+) until "}" | EOF;

        typecheck {
            yield Ty::Void;
        }
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
        typecheck {
            yield Ty::Int;
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
        typecheck {
            yield Ty::Float;
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
        typecheck {
            yield Ty::String;
        }
    }

    rule Entity {
        match ident:Ident;

        typecheck {
            yield find ident as entity;
        }
    }

    rule UnOp {
        match op:Op.* target:Expr[unop];

        typecheck {
            yield target;
        }
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

        typecheck {
            lhs -> rhs;
            yield lhs;
        }
    }

    rule Index {
        match $expr:Expr "[" index:Expr "]";

        typecheck {
            index -> Ty::Int;
            yield Ty::Never;
        }
    }

    rule Call {
        match $expr:Expr "(" args:(:Expr ~ ("," :Expr) until (")") | ("," ")") ","?) unless ")" ")";

        typecheck {
            yield Ty::Never;
        }
    }

    rule Block {
        match "{" list:ExprList "}";

        typecheck (new scope) {
            yield Ty::Never;
        }
    }

    rule VarDecl {
        match "let" name:Ident ty:(?":" :TypeExpr) value:(?"=" :Expr);

        typecheck {
            value -> ty;
            new var(self.name.full_ident(), ty.or(value));
            yield Ty::Void;
        }
    }

    rule If {
        match "if" cond:Expr "{" truthy:Expr "}" falsy:(?"else" :("{" :Expr "}") | If as Expr);

        typecheck {
            cond -> Ty::Bool;
            falsy -> truthy;
            yield truthy;
        }
    }

    rule TypeExpr {
        enum TypeName;

        match :TypeName;
        expected "type";
    }

    rule TypeName {
        match ident:Ident;

        typecheck {
            yield find ident as type;
        }
    }

}
