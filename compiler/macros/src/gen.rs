
extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;
extern crate unicode_xid;
use std::{collections::HashSet, hash::Hash};
use unicode_xid::UnicodeXID;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::{quote, format_ident};
use syn::{
    parse_macro_input,
    parenthesized,
    ImplItemFn,
    Ident,
    Result,
    Token,
    token::{Paren, Bracket},
    parse::{Parse, ParseStream}, 
    LitStr, LitChar,
    braced, Error, ItemUse, Field, ExprBlock,
    punctuated::Punctuated, bracketed, ItemFn,
};

use crate::{clause::{Clause, MaybeBinded, RepeatMode, Char}, defs::Gen, ty::ClauseTy};

pub enum GenCtx {
    None,
    TopLevel {
        is_enum: bool,
        err_branch: TokenStream2,
    },
}

impl Clause {
    pub fn gen_top_prefun(&self, expect_with_name: Ident) -> Result<TokenStream2> {
        match self {
            Self::List { peek_condition, items, rust: _ } => {
                let mut body = TokenStream2::new();
                let mut binded_vars = TokenStream2::new();
                for item in items {
                    if let MaybeBinded::Arg(name, clause) = item {
                        if !peek_condition.is_empty() {
                            return Err(Error::new(Span::call_site(), "cannot have peek conditions with parameter bindings"));
                        }
                        let b = clause.gen()?;
                        body.extend(quote! {
                            let #name = #b;
                        });
                        binded_vars.extend(quote! {
                            #name,
                        });
                    } 
                }
                Ok(quote! {
                    #body
                    Self::#expect_with_name(parser, #binded_vars)
                })
            }
            _ => {
                Err(Error::new(Span::call_site(), "internal error: cant call gen_top_prefun here wtf"))
            }
        }
    }

    pub fn gen_with_ctx(&self, ctx: GenCtx) -> Result<TokenStream2> {
        match self {
            Self::List { peek_condition, items, rust } => {
                let mut body = TokenStream2::new();
                let mut cond = TokenStream2::new();
                for c in peek_condition {
                    let b = c.gen_with_ctx(GenCtx::None)?;
                    cond.extend(quote! {
                        #b;
                    });
                }
                let mut binded_vars = vec![];
                for (i, c) in items.iter().enumerate() {
                    match c {
                        MaybeBinded::Named(name, clause) => {
                            let b = clause.gen()?;
                            body.extend(quote! {
                                let #name = #b;
                            });
                            binded_vars.push(name.clone());
                        }
                        MaybeBinded::Arg(name, _) => {
                            binded_vars.push(name.clone());
                        }
                        MaybeBinded::Unnamed(clause) => {
                            let name = format_ident!("gdml_bind_{i}");
                            let b = clause.gen()?;
                            body.extend(quote! {
                                let #name = #b;
                            });
                            binded_vars.push(name);
                        }
                        MaybeBinded::Drop(clause) => {
                            let b = clause.gen()?;
                            body.extend(quote! {
                                #b;
                            })
                        }
                    }
                }
                let mut result_stream = TokenStream2::new();
                if let Some(rust) = rust {
                    result_stream = quote! { #rust };
                }
                else {
                    match ctx {
                        GenCtx::TopLevel { is_enum, err_branch: _ } => {
                            for r in binded_vars {
                                result_stream.extend(quote! { #r, });
                            }
                            if is_enum {
                                result_stream = quote! { Ok(Self::from(#result_stream)) };
                            }
                            else {
                                result_stream = quote! { Ok(Self {
                                    #result_stream
                                    meta: parser.get_meta(start),
                                }) };
                            }
                        }
                        GenCtx::None => {
                            if binded_vars.len() == 1 {
                                let f = binded_vars.first().unwrap();
                                result_stream.extend(quote! { #f });
                            }
                            else {
                                for r in binded_vars {
                                    result_stream.extend(quote! { #r, });
                                }
                                result_stream = quote! { (#result_stream) };
                            }
                        }
                    }
                }
                cond = quote! {
                    {
                        #cond
                        ()
                    }
                };
                if !peek_condition.is_empty() {
                    match ctx {
                        GenCtx::None => {
                            Ok(quote! {
                                if crate::rule_peek!(parser, #cond) {
                                    let start = parser.skip_ws();
                                    #body
                                    Some(#result_stream)
                                }
                                else {
                                    None
                                }
                            })
                        }
                        GenCtx::TopLevel { is_enum: _, err_branch } => {
                            Ok(quote! {
                                let start = parser.skip_ws();
                                if crate::rule_peek!(parser, #cond) {
                                    #body
                                    #result_stream
                                }
                                else {
                                    #err_branch
                                }
                            })
                        }
                    }
                }
                else {
                    match ctx {
                        GenCtx::None => {
                            Ok(quote! { {
                                let start = parser.skip_ws();
                                #body
                                #result_stream
                            } })
                        }
                        GenCtx::TopLevel { is_enum: _, err_branch } => {
                            Ok(quote! {
                                let start = parser.skip_ws();
                                match crate::rule_try!(parser, {
                                    #body
                                    #result_stream
                                }?) {
                                    Ok(r) => Ok(r),
                                    Err(e) => #err_branch,
                                }
                            })
                        }
                    }
                }
            }
            Self::OneOf(list) => {
                let mut match_options = quote! {
                    let mut furthest_match: Option<(Loc, Message<'s>)> = None;
                };

                let ty = list.first().unwrap().eval_ty()?.gen()?;

                for mat in list {
                    let body = mat.gen()?;
                    match_options.extend(quote! {
                        match crate::rule_try!(parser, #body) {
                            Ok(r) => return Ok(r),
                            Err(e) => {
                                if !furthest_match.as_ref().is_some_and(|m| e.range.end <= m.0) {
                                    furthest_match = Some((e.range.end.clone(), e));
                                }
                            },
                        }
                    });
                }
                
                Ok(quote! {
                    || -> Result<#ty, Message<'s>> {
                        #match_options
                        Err(furthest_match.unwrap().1)
                    }()?
                })
            }
            Self::Option(clause, unless) => {
                let body = clause.gen()?;
                if let Some(unless) = unless {
                    let unless = unless.gen()?;
                    Ok(quote! {
                        if !crate::rule_peek!(parser, #unless) {
                            Some(#body)
                        }
                        else {
                            None
                        }
                    })
                }
                else {
                    Ok(quote! {
                        crate::rule_try!(parser, #body).ok()
                    })
                }
            }
            Self::Concat(list) => {
                let mut stream = quote! {
                    let mut res = String::new();
                };
                for l in list {
                    let b = l.gen()?;
                    if matches!(l.eval_ty()?, ClauseTy::Char) {
                        stream.extend(quote! {
                            res.push(#b);
                        });
                    }
                    else {
                        stream.extend(quote! {
                            res.push_str(&#b);
                        });
                    }
                }
                Ok(quote! {
                    {
                        #stream
                        res
                    }
                })
            }
            Self::ConcatVec(list) => {
                let mut stream = quote! {
                    let mut res = Vec::new();
                };
                for l in list {
                    let b = l.gen()?;
                    stream.extend(quote! {
                        crate::helpers::ConcatInto::concat_into(#b, &mut res);
                    });
                }
                Ok(quote! {
                    {
                        #stream
                        res
                    }
                })
            }
            Self::Repeat(clause, mode) => {
                let body = clause.gen()?;
                // concat chars to a string
                let mut stream = if matches!(clause.eval_ty()?, ClauseTy::Char) {
                    quote! {
                        let mut res = String::new();
                    }
                }
                else {
                    quote! {
                        let mut res = Vec::new();
                    }
                };
                match mode {
                    RepeatMode::OneOrMore | RepeatMode::ZeroOrMore => {
                        if matches!(mode, RepeatMode::OneOrMore) {
                            stream.extend(quote! {
                                res.push(#body);
                            });
                        }
                        Ok(quote! {
                            {
                                #stream
                                while let Ok(b) = crate::rule_try!(parser, #body) {
                                    res.push(b);
                                }
                                res
                            }
                        })
                    }
                    RepeatMode::Until(until) => {
                        let until = until.gen()?;
                        Ok(quote! {
                            {
                                #stream
                                while !crate::rule_peek!(parser, #until) {
                                    res.push(#body);
                                }
                                res
                            }
                        })
                    }
                }
            }
            Self::String(lit) => {
                Ok(quote! {
                    parser.expect_word(#lit)?
                })
            }
            Self::Char(ch) => {
                Ok(match ch {
                    Char::Single(ch) => {
                        quote! {
                            parser.expect_ch(#ch)?
                        }
                    }
                    Char::Not(ch) => {
                        quote! {
                            parser.expect_not_ch(#ch)?
                        }
                    }
                    Char::Range(a, b) => {
                        quote! {
                            parser.expect_ch_range(#a..=#b)?
                        }
                    }
                    Char::XidStart => {
                        quote! {
                            parser.expect_ch_with(UnicodeXID::is_xid_start, "identifier")?
                        }
                    }
                    Char::XidContinue => {
                        quote! {
                            parser.expect_ch_with(UnicodeXID::is_xid_continue, "identifier")?
                        }
                    }
                    Char::OpChar => {
                        quote! {
                            parser.expect_ch_with(crate::parser::is_op_char, "operator")?
                        }
                    }
                    Char::Any => {
                        quote! {
                            parser.expect_ch_with(|_| true, "any")?
                        }
                    }
                    Char::EOF => {
                        quote! {
                            { parser.expect_eof()?; Default::default() }
                        }
                    }
                })
            }
            Self::Rule(rule) => {
                Ok(rule.gen("expect", "expect_impl_", None)?)
            }
            Self::EnumVariant(e, v) => {
                if let Some(v) = v {
                    Ok(quote! {
                        #e::try_from(parser.expect_word(&#e::#v.to_string())?.as_str()).unwrap()
                    })
                }
                else {
                    Ok(quote! {
                        #e::expect_any(parser)?
                    })
                }
            }
            Self::Default => {
                Ok(quote! {
                    Default::default()
                })
            }
            Self::FnMatcher { ret_ty: _, body } => {
                Ok(quote! {
                    #body
                })
            }
        }
    }

    pub fn gen_members(&self) -> Result<TokenStream2> {
        match self {
            Self::List { peek_condition, items, rust } => {
                let mut stream = TokenStream2::new();
                if peek_condition.is_empty() && rust.is_none() {
                    for item in items {
                        if let MaybeBinded::Named(name, clause) | MaybeBinded::Arg(name, clause) = item {
                            let ty = clause.eval_ty()?.gen()?;
                            stream.extend(quote! {
                                #name: #ty,
                            });
                        }
                    }
                }
                Ok(stream)
            }
            _ => Ok(TokenStream2::new())
        }
    }
}

impl Gen for Clause {
    fn gen(&self) -> Result<TokenStream2> {
        self.gen_with_ctx(GenCtx::None)
    }
}
