
use gdml_macros::define_rules;

define_rules! {
    use crate::src::Level;

    keywords {
        "let", "fun", "decl", "struct", "const",
        "is", "as", "from", 
        "if", "else", "for", "while",
        "return", "yield",
        "extern", "export", "import",
        reserve {
            "match", "switch", "mut",
        },
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

        fn path(&self) -> compiler::Path {
            compiler::Path::new([self.value.clone()], false)
        }
    }

    rule Path {
        match absolute:"::"? components:Ident ~ ("::" :Ident)*;

        fn path(&self) -> compiler::Path {
            compiler::Path::new(
                self.components.iter().map(|c| c.value.clone()).collect::<Vec<_>>(),
                self.absolute.is_some()
            )
        }
    }

    rule ExprList {
        match exprs:(:Expr ";"+ | (<='}' :";"*)) until "}" | EOF;

        typecheck {
            yield Ty::Void;
        }
    }

    rule Expr {
        enum If, VarDecl, ConstDecl, FunDecl, Yield, Return, Block, Float, Int, Str, Entity, BinOp, UnOp, Index, Call;

        match :BinOp;
        match[unop] ??OP_CHAR :UnOp;
        match[postfix] :Expr[nonop] afterwards
            while_peek "(" into Call
            while_peek "[" into Index;
        match[nonop] ??"if" :If;
        match ??"let" :VarDecl;
        match ??"const" :ConstDecl;
        match ??"fun" :FunDecl;
        match ??"yield" :Yield;
        match ??"return" :Return;
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
        match path:Path;

        typecheck {
            yield find path as entity;
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
            yield {
                match checker.binop_ty(&lhs, self.op, &rhs) {
                    Some(ty) => ty,
                    None => {
                        checker.emit_msg(Message::from_meta(
                            Level::Error,
                            format!("Cannot apply '{}' to '{lhs}' and '{rhs}'", self.op),
                            self.meta()
                        ));
                        Ty::Invalid
                    }
                }
            };
        }
    }

    rule Index {
        match $expr:Expr "[" index:Expr "]";

        typecheck {
            index -> Ty::Int;
            yield Ty::Void;
        }
    }

    rule Call {
        match $expr:Expr "(" args:(:Expr ~ ("," :Expr) until (")") | ("," ")") ","?) unless ")" ")";

        typecheck {
            yield eval {
                Ty::Function {
                    params: args.into_iter().map(|a| (String::new(), a)).collect(),
                    ret_ty: Ty::Inferred.into(),
                    decl: self.as_ref(),
                }
            } -> expr;
        }
    }

    rule Yield {
        match "yield" expr:Expr?;

        typecheck {
            return expr from opaque;
            yield Ty::Invalid; // todo: yield Never
        }
    }

    rule Return {
        match "return" expr:Expr?;

        typecheck {
            return expr from function;
            yield Ty::Invalid; // todo: yield Never
        }
    }

    rule Block {
        match "{" list:ExprList "}";

        typecheck (manual) {
            scope {
                check list;
            };
            yield Ty::Void;
        }
    }

    rule ConstDecl {
        match "const" name:Ident ty:(?":" :TypeExpr) value:(?"=" :Expr);

        typecheck {
            value -> ty;
            new entity name: ty.or(value).unwrap_or(Ty::Inferred);
            yield Ty::Void;
        }
    }

    rule VarDecl {
        match "let" name:Ident ty:(?":" :TypeExpr) value:(?"=" :Expr);

        typecheck {
            value -> ty;
            new entity mut name: ty.or(value).unwrap_or(Ty::Inferred);
            yield Ty::Void;
        }
    }

    rule FunParamDecl {
        match name: Ident ":" ty:TypeExpr;

        typecheck {
            yield new entity name: ty;
        }
    }

    rule FunDecl {
        match "fun" name:Ident? "("
            params:(:FunParamDecl ~ ("," :FunParamDecl) until (")") | ("," ")") ","?) unless ")"
        ")" "->" ret_ty:TypeExpr body:Block as Expr? | ("=>" :Expr)?;

        typecheck (manual) {
            check name;
            check ret_ty;
            scope function {
                check params;
                check body;
                body -> ret_ty;
            };
            yield new entity name?: Ty::Function {
                params: self.params.iter()
                    .zip(params)
                    .map(|(p, ty)| (p.name.value.clone(), ty))
                    .collect(),
                ret_ty: ret_ty.into(),
                decl: self.into(),
            };
        }
    }

    rule If {
        match "if" cond:Expr "{" truthy:ExprList "}" falsy:(?"else" :as Expr (Block | If));

        typecheck (manual) {
            scope {
                // check cond and truthy in same scope so any let bindings in 
                // the condition are available inside truthy
                check cond;
                check truthy;
            };
            check falsy;
            cond -> Ty::Bool for cond;
            // both branches must result in the same type
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

pub use parse_op_impl as parse_op;
