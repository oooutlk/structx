extern crate proc_macro;
use proc_macro::{Delimiter, Group, TokenStream, TokenTree};

extern crate proc_macro2;
use proc_macro2::Span;

use quote::{quote, ToTokens};

use std::mem;

use syn::spanned::Spanned;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::Colon,
    visit_mut::{self, VisitMut},
    ExprStruct, FnArg, Ident, ItemFn, Member, Pat, PatStruct, Token, Type,
};

/*
 * Introduced a new type which can be parsed with syn::parse2.
 * This is necessary because syn version 2 doesn't implement Parse for Pat
 */
struct StructX {
    pat: PatStruct,
}

impl Parse for StructX {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = Pat::parse_single(input)?;
        if let Pat::Struct(pat) = pat {
            Ok(StructX { pat })
        } else {
            Err(syn::Error::new(
                pat.span(),
                "structx!()'s supported pattern matching is struct only.",
            ))
        }
    }
}

/// Value of anonymous struct.
#[proc_macro]
pub fn structx(input: TokenStream) -> TokenStream {
    let input_expr = wrap_struct_name("structx_", input.clone());
    if let Ok(expr_struct) = syn::parse::<ExprStruct>(input_expr) {
        if let Some(rest) = &expr_struct.rest {
            let rest = &**rest;
            let specified_idents = expr_struct.fields.iter().map(|field| {
                if let Member::Named(field_ident) = &field.member {
                    field_ident
                } else {
                    panic!();
                }
            });
            let specified_exprs = expr_struct.fields.iter().map(|field| &field.expr);
            return quote!({ let mut _rest = #rest; #( _rest.#specified_idents = #specified_exprs; )* _rest }).into();
        } else {
            let (struct_name, _, _) =
                join_field_members(expr_struct.fields.iter().map(|field| &field.member));
            return wrap_struct_name(&struct_name, input);
        }
    } else {
        let input_pat = wrap_struct_name("structx_", input.clone());
        if let Ok(struct_x) = syn::parse::<StructX>(input_pat) {
            let (struct_name, _, _) =
                join_field_members(struct_x.pat.fields.iter().map(|field| &field.member));
            return wrap_struct_name(&struct_name, input);
        }
    }
    panic!("structx!() should be some struct.");
}

struct StructxField {
    ident: Ident,
    _colon: Colon,
    ty: Type,
}

impl Parse for StructxField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(StructxField {
            ident: input.parse()?,
            _colon: input.parse()?,
            ty: input.parse()?,
        })
    }
}

struct StructxType(Punctuated<StructxField, Token![,]>);

impl Parse for StructxType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(StructxType(
            Punctuated::<StructxField, Token![,]>::parse_terminated(input)?,
        ))
    }
}

/// Type of anonymous struct.
#[allow(non_snake_case)]
#[proc_macro]
pub fn Structx(input: TokenStream) -> TokenStream {
    let structx_type = syn::parse::<StructxType>(input).expect(
        "`Structx!{}` should be in the form of `Structx!{ field0: Type0, field1: Type1, .. }`.",
    );
    let (struct_name, _, field_types) = join_fields(
        structx_type
            .0
            .into_iter()
            .map(|structx_field| (structx_field.ident, Some(structx_field.ty))),
    );

    let struct_ident = Ident::new(&struct_name, Span::call_site());
    quote!( #struct_ident<#( #field_types),*> ).into()
}

fn wrap_struct_name(struct_name: &str, input: TokenStream) -> TokenStream {
    let mut ts = TokenStream::from(Ident::new(struct_name, Span::call_site()).into_token_stream());
    ts.extend(Some(TokenTree::Group(Group::new(Delimiter::Brace, input))));
    ts
}

fn join_fields(
    fields: impl Iterator<Item = (Ident, Option<Type>)>,
) -> (String, Vec<Ident>, Vec<Option<Type>>) {
    let mut fields = fields.collect::<Vec<_>>();
    fields.sort_by_key(|field| field.0.clone());
    fields.into_iter().fold(
        ("structx".to_owned(), Vec::new(), Vec::new()),
        |(mut struct_name, mut field_idents, mut field_types), (ident, ty)| {
            let field_name = ident.to_string();
            struct_name.push('_');
            struct_name.push_str(&field_name.replace("_", "__"));
            field_idents.push(ident.clone());
            field_types.push(ty);
            (struct_name, field_idents, field_types)
        },
    )
}

fn join_field_members<'a>(
    members: impl Iterator<Item = &'a Member>,
) -> (String, Vec<Ident>, Vec<Option<Type>>) {
    join_fields(
        members
            .map(|member| {
                if let Member::Named(ident) = member {
                    ident.clone()
                } else {
                    panic!("structx!()'s fields should have names.");
                }
            })
            .zip((0..).map(|_| None)),
    )
}

struct FnWithNamedArg;

impl VisitMut for FnWithNamedArg {
    fn visit_item_fn_mut(&mut self, item_fn: &mut ItemFn) {
        visit_mut::visit_item_fn_mut(self, item_fn);

        let inputs = mem::take(&mut item_fn.sig.inputs);
        let fn_args = inputs.into_iter();
        let mut idents = Vec::with_capacity(fn_args.len());
        let mut types = Vec::with_capacity(fn_args.len());

        for fn_arg in fn_args {
            match fn_arg {
                FnArg::Receiver(_) => item_fn.sig.inputs.push(fn_arg),
                FnArg::Typed(pat_type) => {
                    if let Pat::Ident(pat_ident) = &*pat_type.pat {
                        idents.push(pat_ident.ident.clone());
                        types.push(pat_type.ty);
                    } else {
                        panic!("#[named_args] function's arguments should be either receiver or `id: Type`.");
                    }
                }
            }
        }
        let (struct_name, field_idents, field_types) = join_fields(
            idents
                .into_iter()
                .zip(types.into_iter().map(|ty| Some(*ty))),
        );
        let struct_ident = Ident::new(&struct_name, Span::call_site());

        item_fn.sig.inputs.push(parse_quote!(
            #struct_ident {
                #( #field_idents ),*
            } : #struct_ident< #(#field_types),*>
        ));
    }
}

/// Simulation of named arguments.
#[proc_macro_attribute]
pub fn named_args(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_fn = parse_macro_input!(input as ItemFn);
    FnWithNamedArg.visit_item_fn_mut(&mut item_fn);
    quote!( #item_fn ).into()
}
