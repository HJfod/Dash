use crate::{ast::*, utils::File, output::Output};

peg::parser!{
    pub grammar dash_grammar(file: &Box<File>, channel: &Output) for str {
        rule _ = quiet! { (" " / "\r" / "\n" / "\t")* }

        rule escaped() -> char
            = "\\" c:['n' | 't' | '0' | '\"' | '\\'] {
                match c {
                    'n'  => '\n',
                    't'  => '\t',
                    '0'  => '\0',
                    '\\' => '\\',
                    '"'  => '\"',
                    _    => ' '
                }
            }

        rule integer() -> PNode
            = start:position!() n:$("-"? ['0'..='9']+) end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::Int(
                        n.parse::<i64>().expect(
                            "Internal compiler error: Invalid integer literal"
                        )
                    ),
                    channel
                )
            }
        
        rule float() -> PNode
            = start:position!() n:$("-"? ['0'..='9']+ "." ['0'..='9']+) end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::Float(
                        n.parse::<f64>().expect(
                            "Internal compiler error: Invalid float literal"
                        )
                    ),
                    channel
                )
            }

        rule string() -> PNode
            = start:position!() "\"" v:(escaped() / [^ '\"'])* "\"" end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::String(v.iter().cloned().collect::<String>()),
                    channel
                )
            }
        
        rule literal() -> PNode
            = quiet!{ float() / integer() / string() } / expected!("literal")

        rule identifier() -> String
            = quiet!{
                i:$(['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) {
                    String::from(i)
                }
            } / expected!("identifier")
        
        rule variable() -> PNode
            = start:position!() i:identifier() end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::Iden(i),
                    channel
                )
            }

        rule value() -> PNode
            = literal() / variable() / "(" _ n:expr() _ ")" { n }
        
        rule bin_op() -> PNode
            = precedence!{
                start:position!() node:@ end:position!() {
                    Node::new(
                        file, start, end, node,
                        channel
                    )
                }
                --
                a:(@) _ "+" _ b:@ {
                    NodeKind::Binary(BinOp::Add, a, b)
                }
                a:(@) _ "-" _ b:@ {
                    NodeKind::Binary(BinOp::Sub, a, b)
                }
                --
                a:(@) _ "*" _ b:@ {
                    NodeKind::Binary(BinOp::Mul, a, b)
                }
                a:(@) _ "/" _ b:@ {
                    NodeKind::Binary(BinOp::Div, a, b)
                }
                --
                a:(@) _ "**" _ b:@ {
                    NodeKind::Binary(BinOp::Pow, a, b)
                }
                --
                a:(@) _ "=" _ b:@ {
                    NodeKind::Binary(BinOp::Assign, a, b)
                }
                a:(@) _ "+=" _ b:@ {
                    NodeKind::Binary(BinOp::AddAssign, a, b)
                }
                a:(@) _ "-=" _ b:@ {
                    NodeKind::Binary(BinOp::SubAssign, a, b)
                }
                a:(@) _ "*=" _ b:@ {
                    NodeKind::Binary(BinOp::MulAssign, a, b)
                }
                a:(@) _ "/=" _ b:@ {
                    NodeKind::Binary(BinOp::DivAssign, a, b)
                }
                --
                "+" _ a:(@) {
                    NodeKind::Unary(UnaryOp::Plus, a)
                }
                "-" _ a:(@) {
                    NodeKind::Unary(UnaryOp::Minus, a)
                }
                "!" _ a:(@) {
                    NodeKind::Unary(UnaryOp::Not, a)
                }
                --
                a:(@) _ "(" _ p:(f:expr() m:(_ "," _ e:expr() { e })* _ ","? {
                    vec!(f).into_iter().chain(m.into_iter()).collect::<Vec<_>>()
                }) _ ")" {
                    NodeKind::Call(a, p)
                }
                --
                v:value() { v.kind().to_owned() }
            }
        
        rule type_expr() -> PNode
            = start:position!() i:identifier() end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::Iden(i),
                    channel
                )
            }
        
        rule expr() -> PNode
            = a:bin_op() { a }

        rule var_decl() -> PNode
            =   
                start:position!()
                i:identifier()
                t:(_ ":" _ v:type_expr() { v })?
                o:(_ "=" _ v:expr() { v })?
                end:position!()
                {
                    Node::new(
                        file, start, end,
                        NodeKind::DeclVar(i, t, o),
                        channel
                    )
                }
        
        rule let_var_decl() -> PNode
            = start:position!() "let " _ v:var_decl() end:position!() { v }
     
        rule fun_decl() -> PNode
            = 
                start:position!()
                "fun " _ name:identifier()
                _ "(" _ params:(
                    p:var_decl() m:(_ "," _ p:var_decl() { p })* ","? {
                        vec!(p).into_iter().chain(m.into_iter()).collect::<Vec<_>>()
                    }
                )? _ ")"
                ret_type:(_ "->" _ t:type_expr() { t })?
                body:(
                    // { body }
                    _ "{" _ l:stmt_list(false) _ "}" {
                        Some(NodePtr::from(l))
                    } /
                    // => expr (shorthand)
                    start:position!() _ "=>" _ s:expr() _ ";" end:position!() {
                        Some(Node::new(
                            file, start, end,
                            NodeKind::Return(Some(s)),
                            channel
                        ))
                    } /
                    // no body
                    ";" { None }
                )
                end:position!()
                {
                    Node::new(
                        file, start, end,
                        NodeKind::DeclFun(name, params.unwrap_or(vec!()), ret_type, body),
                        channel
                    )
                }
        
        rule return_stmt() -> PNode
            = start:position!() "return " _ e:expr()? end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::Return(e),
                    channel
                )
            }
        
        rule stmt(top_level: bool) -> PNode
            = s:let_var_decl() ";" { s } /
              fun_decl() /
              s:return_stmt() ";" { s } /
              "{" _ s:stmt_list(top_level) _ "}" { s } /
              e:expr() ";" { e }
        
        rule stmt_list(top_level: bool) -> PNode
            = start:position!() l:(_ s:stmt(top_level) { s })* end:position!() {
                Node::new(
                    file, start, end,
                    NodeKind::List(l),
                    channel
                )
            }

        pub rule parse() -> PNode
            = _ s:stmt_list(true) _ { s }
    }
}

