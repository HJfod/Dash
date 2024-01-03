
extern crate proc_macro;
extern crate proc_macro2;
extern crate syn;
extern crate quote;
extern crate darling;

use darling::{FromDeriveInput, ast, FromField, FromVariant};
use darling::{FromMeta, ast::NestedMeta};
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Ident};
use quote::{quote, quote_spanned, ToTokens, format_ident};
use syn::parse::Parse;
use syn::{Generics, Type, Path};
use syn::spanned::Spanned;
use syn::{parse_macro_input, ItemStruct, parse::Parser, Fields, Field};

// https://stackoverflow.com/questions/55271857/how-can-i-get-the-t-from-an-optiont-when-using-syn
fn extract_type_from_option(ty: &syn::Type) -> Option<&syn::Type> {
    use syn::{GenericArgument, PathArguments, PathSegment};

    fn extract_type_path(ty: &syn::Type) -> Option<&Path> {
        match *ty {
            syn::Type::Path(ref typepath) if typepath.qself.is_none() => Some(&typepath.path),
            _ => None,
        }
    }

    // TODO store (with lazy static) the vec of string
    // TODO maybe optimization, reverse the order of segments
    fn extract_option_segment(path: &Path) -> Option<&PathSegment> {
        let idents_of_path = path
            .segments
            .iter()
            .fold(String::new(), |mut acc, v| {
                acc.push_str(&v.ident.to_string());
                acc.push('|');
                acc
            });
        vec!["Option|", "std|option|Option|", "core|option|Option|"]
            .into_iter()
            .find(|s| idents_of_path == *s)
            .and_then(|_| path.segments.last())
    }

    extract_type_path(ty)
        .and_then(|path| extract_option_segment(path))
        .and_then(|path_seg| {
            let type_params = &path_seg.arguments;
            // It should have only on angle-bracketed param ("<String>"):
            match *type_params {
                PathArguments::AngleBracketed(ref params) => params.args.first(),
                _ => None,
            }
        })
        .and_then(|generic_arg| match *generic_arg {
            GenericArgument::Type(ref ty) => Some(ty),
            _ => None,
        })
}

macro_rules! unwrap_macro_input {
    ($e: expr) => {
        match $e {
            Ok(v) => v,
            Err(e) => return syn::Error::from(e).to_compile_error().into(),
        }
    };
}

macro_rules! get_named_fields {
    ($from: ident as $($borrow: tt)+) => {
        match $($borrow)+$from.fields {
            Fields::Named(named) => $($borrow)+named.named,
            _ => {
                return syn::Error::new(
                    $from.ident.span(), "AST nodes must be structs with named fields"
                ).to_compile_error().into()
            }
        }
    };
    (&$from: ident) => {
        get_named_fields!($from as &)
    };
    (&mut $from: ident) => {
        get_named_fields!($from as &mut)
    };
}

fn impl_ast_item(
    target: &impl ToTokens, target_name: &Ident, target_generics: &Generics,
    parse_impl: TokenStream2, peek_impl: TokenStream2, span_impl: TokenStream2
) -> TokenStream2 {
    let (impl_generics, ty_generics, where_clause) = target_generics.split_for_impl();
    let type_name = match target_name.to_string().strip_suffix("Item") {
        Some(n) => format_ident!("{n}"),
        None => return syn::Error::new(
            target_name.span(),
            "the name of a Parsed class should be suffixed with 'Item'"
        ).to_compile_error(),
    };
    quote! {
        #target
        impl #impl_generics crate::parser::parse::Node for #target_name #ty_generics #where_clause {
            fn span(&self) -> Option<crate::shared::src::ArcSpan> {
                #span_impl
            }
        }
        impl #impl_generics crate::parser::parse::Parse for #target_name #ty_generics #where_clause {
            fn parse<'s>(
                list: &mut crate::parser::parse::NodeList,
                src: std::sync::Arc<crate::shared::src::Src>,
                tokenizer: &mut crate::parser::tokenizer::TokenIterator<'s>
            ) -> Result<Self, crate::parser::parse::FatalParseError>
                where Self: Sized
            {
                #parse_impl
            }
            fn peek<'s>(
                pos: usize,
                tokenizer: &crate::parser::tokenizer::TokenIterator<'s>
            ) -> bool
                where Self: Sized
            {
                #peek_impl
            }
        }
        pub type #type_name #impl_generics = crate::parser::parse::RefToNode<#target_name #ty_generics>;
    }
}

fn impl_ast_struct(
    target: &mut ItemStruct,
    parse_impl: TokenStream2,
    peek_impl: TokenStream2,
    span_impl: TokenStream2
) -> TokenStream {
    get_named_fields!(&mut target).push(
        Field::parse_named.parse2(quote! { span: crate::shared::src::ArcSpan }).unwrap()
    );
    impl_ast_item(target, &target.ident, &target.generics, parse_impl, peek_impl, span_impl).into()
}

#[derive(Debug, FromMeta)]
struct TokenArgs {
    kind: String,
    raw: Option<String>,
    #[darling(default)]
    value_is_token_tree: bool,
    #[darling(default)]
    include_raw: bool,
    #[darling(default)]
    new_builtin: bool,
}

#[proc_macro_attribute]
pub fn token(args: TokenStream, stream: TokenStream) -> TokenStream {
    let mut target = parse_macro_input!(stream as ItemStruct);
    let args = unwrap_macro_input!(TokenArgs::from_list(
        &unwrap_macro_input!(NestedMeta::parse_meta_list(args.into()))
    ));
    target.ident = format_ident!("{}Item", target.ident);
    let expected_construct;
    let value_field;
    let destruct_kind;
    let destruct_drop;
    if let Some(path) = args.kind.strip_suffix("(_)") {
        let path = Ident::new(path, args.kind.span());
        if args.value_is_token_tree {
            expected_construct = quote!{ #path(tokenizer.empty_tree()) };
            value_field = quote! { value: crate::parser::parse::Parse::parse_complete(list, src.clone(), value)?, };
        }
        else {
            expected_construct = quote!{ #path(Default::default()) };
            value_field = quote! { value: value.into(), };
        }
        destruct_drop = quote! { #path(_) };
        destruct_kind = quote! { #path(value) };
    }
    else {
        let path = Ident::new(&args.kind, args.kind.span());
        expected_construct = quote! { #path };
        value_field = quote! {};
        destruct_drop = quote! { #path };
        destruct_kind = quote! { #path };
    }
    let expected_kind = {
        let raw = args.raw.as_deref().unwrap_or("");
        quote! { crate::parser::tokenizer::Token {
            kind: crate::parser::tokenizer::TokenKind::#expected_construct,
            raw: #raw,
            span: crate::shared::src::Span::builtin(),
        } }
    };
    let test_raw = if let Some(ref raw) = args.raw {
        quote! { peek.raw == #raw }
    }
    else {
        quote! { true }
    };
    let raw_field = if args.include_raw {
        get_named_fields!(&mut target).push(
            Field::parse_named.parse2(quote! { raw: String }).unwrap()
        );
        quote! { raw: token.raw.to_string(), }
    }
    else {
        quote! {}
    };
    let r: TokenStream2 = impl_ast_struct(
        &mut target,
        quote! {
            use crate::parser::tokenizer::TokenKind;
            use crate::shared::src::ArcSpan;
            if let Some(peek) = tokenizer.peek(0) {
                if let TokenKind::#destruct_drop = peek.kind {
                    if #test_raw {
                        let token = tokenizer.next().unwrap();
                        let TokenKind::#destruct_kind = token.kind else { unreachable!() };
                        return Ok(Self {
                            #value_field
                            #raw_field
                            span: ArcSpan(src, token.span.1)
                        });
                    }
                }
            }
            tokenizer.expected(#expected_kind);
            Err(crate::parser::parse::FatalParseError)
        },
        quote! {
            use crate::parser::tokenizer::TokenKind;
            if let Some(peek) = tokenizer.peek(pos) {
                matches!(peek.kind, TokenKind::#destruct_drop) && #test_raw
            }
            else {
                false
            }
        },
        quote! {
            Some(self.span.clone())
        }
    ).into();
    let name = target.ident;
    let (impl_generics, ty_generics, where_clause) = target.generics.split_for_impl();
    let impl_new_default = if args.new_builtin {
        let raw_field = if args.include_raw {
            if let Some(raw) = args.raw {
                quote! { raw: String::from(#raw), }
            }
            else {
                quote! { raw: String::new(), }
            }
        }
        else {
            quote! {}
        };
        quote! {
            impl #impl_generics #name #ty_generics #where_clause {
                pub fn builtin() -> Self {
                    Self {
                        #raw_field
                        span: crate::shared::src::ArcSpan::builtin(),
                    }
                }
            }
        }
    }
    else {
        quote! {}
    };
    quote! {
        #[derive(Debug)]
        #r
        impl #impl_generics crate::parser::parse::IsToken for #name #ty_generics #where_clause {}
        #impl_new_default
    }.into()
}

#[derive(FromDeriveInput)]
#[darling(attributes(parse), supports(any))]
struct ParseReceiver {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<ParseVariant, ParseField>,
    expected: Option<String>,
    #[darling(default)]
    no_peek: bool,
}

#[derive(FromVariant)]
struct ParseVariant {
    ident: syn::Ident,
    fields: ast::Fields<ParseField>,
}

#[derive(FromField)]
#[darling(attributes(parse))]
struct ParseField {
    ident: Option<syn::Ident>,
    ty: Type,
    #[darling(default)]
    peek_point: bool,
    #[darling(default)]
    skip: bool,
    #[darling(default)]
    skip_with: Option<String>,
}

fn field_to_tokens(data: &ast::Fields<ParseField>, self_name: Path) -> (TokenStream2, TokenStream2, TokenStream2) {
    let mut span_impl = quote! {};
    let mut parse_impl = quote! {};
    let mut peek_checks = quote! {};
    let mut peek_impl = quote! {};

    // Find peek point if it was manually set
    let mut encountered_peek_end = false;
    let mut peek_count = 0;
    for field in data.iter().filter(|d| !d.skip && d.skip_with.is_none()) {
        if field.peek_point {
            if extract_type_from_option(&field.ty).is_some() {
                parse_impl.extend(
                    syn::Error::new(
                        field.ty.span(),
                        "peek point may not be an optional field"
                    ).to_compile_error()
                );
            }
            if encountered_peek_end {
                parse_impl.extend(
                    syn::Error::new(
                        field.ident.span(),
                        "only one field may be marked as the peek point"
                    ).to_compile_error()
                );
            }
            encountered_peek_end = true;
            peek_count += 1;
        }
        else if !encountered_peek_end {
            peek_count += 1;
        }
    }
    // Automatically figure out peek point if it wasn't manually set
    if !encountered_peek_end {
        peek_count = 0;
        for field in data.iter() {
            // Break unless the type is optional, in which case 
            // continue peeking since an optional field is not 
            // enough to determine exhaustively
            if extract_type_from_option(&field.ty).is_none() {
                peek_count += 1;
                break;
            }
        }
    }

    peek_checks.extend(quote! {
        static_assertions::const_assert!(
            #peek_count <= crate::parser::tokenizer::MAX_PEEK_COUNT
        );
    });
    
    // Generate parse and peek impls
    let mut peek_ix = 0usize;
    for (field_ix, field) in data.iter().enumerate() {
        if let Some(ref skip) = field.skip_with {
            match syn::Expr::parse.parse_str(skip) {
                Ok(skip) => {
                    if let Some(ref i) = field.ident {
                        parse_impl.extend(quote! { #i: #skip, });
                    }
                    else {
                        parse_impl.extend(quote! { #skip, });
                    }
                }
                Err(e) => {
                    let e = e.to_compile_error().to_token_stream();
                    parse_impl.extend(quote_spanned!(skip.span() => #e));
                }
            }
        }
        else if field.skip {
            if let Some(ref i) = field.ident {
                parse_impl.extend(quote! { #i: Default::default(), });
            }
            else {
                parse_impl.extend(quote! { Default::default(), });
            }
        }
        else {
            let t = &field.ty;
            if let Some(ref i) = field.ident {
                parse_impl.extend(quote! {
                    #i: Parse::parse(list, src.clone(), tokenizer)?,
                });
                span_impl.extend(quote! { self.#i.span(), });
            }
            else {
                parse_impl.extend(quote! {
                    Parse::parse(list, src.clone(), tokenizer)?,
                });
                span_impl.extend(quote! { self.#field_ix.span(), });
            }
            if peek_ix < peek_count {
                // if we are peeking more than 1 member, all but last must be 
                // tokens
                if peek_ix < peek_count - 1 {
                    peek_checks.extend(quote_spanned! {
                        t.span() => <#t as crate::parser::parse::IsToken>::is_token();
                    });
                }
                peek_impl.extend(quote! {
                    if <#t>::peek(#peek_ix, tokenizer) {
                        peeked += 1;
                    }
                });
                if extract_type_from_option(t).is_none() {
                    peek_ix += 1;
                }
            }
        }
    }
    (
        if data.is_struct() {
            quote! {
                use crate::parser::parse::Node;
                use crate::shared::src::ArcSpan;
                Ok(#self_name {
                    #parse_impl
                })
            }
        }
        else if data.is_unit() {
            quote! {
                Ok(#self_name)
            }
        }
        else {
            quote! {
                use crate::parser::parse::Node;
                use crate::shared::src::ArcSpan;
                Ok(#self_name(
                    #parse_impl
                ))
            }
        },
        quote! {
            #peek_checks
            let mut peeked = 0;
            #peek_impl
            peeked == #peek_count
        },
        quote! {
            crate::parser::parse::calculate_span([#span_impl])
        }
    )
}

impl ToTokens for ParseReceiver {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match &self.data {
            ast::Data::Struct(data) => {
                if self.expected.is_some() {
                    tokens.extend(
                        syn::Error::new(
                            self.expected.span(),
                            "cannot use \"expected\" on a struct"
                        ).to_compile_error()
                    );
                }
                // note to self: don't call `self.span()` - it causes rustc to crash
                let (parse, peek, span) = field_to_tokens(
                    data, Path::from_string("Self").unwrap()
                );
                tokens.extend(impl_ast_item(
                    &quote!{}, &self.ident, &self.generics,
                    parse,
                    if self.no_peek { quote! { false } } else { peek },
                    span
                ));
            }
            ast::Data::Enum(data) => {
                let mut parse_impl = quote! {};
                let mut peek_impl = quote! {};
                let mut span_impl = quote! {};
                for variant in data {
                    let v = &variant.ident;
                    if variant.fields.is_unit() {
                        span_impl.extend(quote! { Self::#v => None, });
                        // No peeking or parsing unit variants
                    }
                    else {
                        let (parse, peek, _) = field_to_tokens(
                            &variant.fields,
                            Path::from_string(&format!("Self::{v}")).unwrap()
                        );
                        parse_impl.extend(quote! {
                            if { #peek } {
                                return { #parse };
                            }
                        });
                        peek_impl.extend(quote! {
                            if { #peek } {
                                return true;
                            }
                        });
                        let destruct;
                        let mut names = quote! {};
                        let mut spans = quote! {};
                        if variant.fields.is_struct() {
                            for field in variant.fields.fields.iter() {
                                let name = &field.ident;
                                if field.skip || field.skip_with.is_some() {
                                    names.extend(quote! { #name: _, });
                                }
                                else {
                                    names.extend(quote! { #name, });
                                    spans.extend(quote! { #name.span(), });
                                }
                            }
                            destruct = quote! { {#names} };
                        }
                        else {
                            // todo: parse and peek impls
                            for (field, c) in variant.fields.fields.iter().zip(
                                ('a'..='z').map(|c| Ident::new(&c.to_string(), v.span()))
                            ) {
                                if field.skip || field.skip_with.is_some() {
                                    names.extend(quote! { _, });
                                }
                                else {
                                    names.extend(quote! { #c, });
                                    spans.extend(quote! { #c.span(), });
                                }
                            }
                            destruct = quote! { (#names) };
                        };
                        span_impl.extend(quote! {
                            Self::#v #destruct => crate::parser::parse::calculate_span([#spans]),
                        });
                    }
                }
                
                let expected = &self.expected;
                tokens.extend(impl_ast_item(
                    &quote!{}, &self.ident, &self.generics,
                    quote! {
                        use crate::parser::parse::Parse;
                        #parse_impl
                        tokenizer.expected(#expected);
                        Err(crate::parser::parse::FatalParseError)
                    },
                    if self.no_peek {
                        quote! { false }
                    }
                    else {
                        quote! {
                            #peek_impl
                            false
                        }
                    },
                    quote! {
                        match self {
                            #span_impl
                        }
                    }
                ));
            }
        }
    }
}

#[proc_macro_derive(Parse, attributes(parse))]
pub fn derive_parse(input: TokenStream) -> TokenStream {
    match ParseReceiver::from_derive_input(&syn::parse(input).expect("Couldn't parse item")) {
        Ok(v) => v,
        Err(e) => {
            return e.write_errors().into();
        }
    }.to_token_stream().into()
}

#[derive(FromDeriveInput)]
#[darling(supports(enum_newtype))]
struct ResolveReceiver {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<ResolveVariant, ()>,
}

#[derive(FromVariant)]
struct ResolveVariant {
    ident: syn::Ident,
}

impl ToTokens for ResolveReceiver {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let try_resolve;

        match &self.data {
            ast::Data::Struct(_) => {
                unimplemented!("structs not yet supported")
            }
            ast::Data::Enum(data) => {
                let mut try_resolve_matches = quote! {};
                for v in data {
                    let ident = &v.ident;
                    try_resolve_matches.extend(quote_spanned! {
                        v.ident.span() =>
                        Self::#ident(value) => crate::checker::resolve::Resolve::try_resolve(value, list, checker),
                    });
                }
                try_resolve = quote! {
                    match self {
                        #try_resolve_matches
                    }
                };
            }
        }

        let name = &self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        tokens.extend(quote! {
            impl #impl_generics crate::checker::resolve::Resolve for #name #ty_generics #where_clause {
                fn try_resolve(
                    &mut self,
                    list: &mut crate::parser::parse::NodeList,
                    checker: &mut crate::checker::coherency::Checker
                ) -> Option<crate::checker::ty::Ty> {
                    #try_resolve
                }
            }
        });
    }
}

#[proc_macro_derive(Resolve)]
pub fn derive_resolve(input: TokenStream) -> TokenStream {
    match ResolveReceiver::from_derive_input(&syn::parse(input).expect("Couldn't parse item")) {
        Ok(v) => v,
        Err(e) => {
            return e.write_errors().into();
        }
    }.to_token_stream().into()
}
