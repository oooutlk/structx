extern crate proc_macro;
use proc_macro::{
    Delimiter,
    Group,
    TokenStream,
    TokenTree,
};

extern crate proc_macro2;
use proc_macro2::Span;

use quote::{
    ToTokens,
    quote,
};

use std::{
    collections::HashMap,
    fs,
    mem,
};

use syn::{
    ExprStruct,
    FnArg,
    Ident,
    ItemFn,
    ItemStruct,
    Macro,
    Member,
    Pat,
    PathArguments,
    Token,
    Type,
    parse_macro_input,
    parse_quote,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Colon,
    visit::{self, Visit},
    visit_mut::{self, VisitMut},
};

/// Value of anonymous struct.
#[proc_macro]
pub fn structx( input: TokenStream ) -> TokenStream {
    let input_expr = wrap_struct_name( "structx_", input.clone() );
    if let Ok( expr_struct ) = syn::parse::<ExprStruct>( input_expr ) {
        let (struct_name, _, _) = join_field_members( expr_struct.fields.iter().map( |field| &field.member ));
        return wrap_struct_name( &struct_name, input );
    } else {
        let input_pat = wrap_struct_name( "structx_", input.clone() );
        if let Ok( pat ) = syn::parse::<Pat>( input_pat ) {
            if let Pat::Struct( pat_struct ) = pat {
                let (struct_name, _, _) = join_field_members( pat_struct.fields.iter().map( |field| &field.member ));
                return wrap_struct_name( &struct_name, input );
            }
        }
    }
    panic!("structx!() should be some struct.");
}

struct StructxField {
    ident  : Ident,
    _colon : Colon,
    ty     : Type,
}

impl Parse for StructxField {
    fn parse( input: ParseStream ) -> syn::Result<Self> {
        Ok( StructxField {
            ident  : input.parse()?,
            _colon : input.parse()?,
            ty     : input.parse()?,
        })
    }
}

struct StructxType( Punctuated<StructxField, Token![,]> );

impl Parse for StructxType {
    fn parse( input: ParseStream ) -> syn::Result<Self> {
        Ok( StructxType( Punctuated::<StructxField, Token![,]>::parse_terminated( input )? ))
    }
}

/// Type of anonymous struct.
#[allow( non_snake_case )]
#[proc_macro]
pub fn Structx( input: TokenStream ) -> TokenStream {
    let structx_type = syn::parse::<StructxType>( input )
        .expect("`Structx!{}` should be in the form of `Structx!{ field0: Type0, field1: Type1, .. }`.");
    let (struct_name, _, field_types) =
        join_fields( structx_type.0.into_iter().map( |structx_field| (structx_field.ident, Some( structx_field.ty ))));

    let struct_ident = Ident::new( &struct_name, Span::call_site() );
    quote!( #struct_ident<#( #field_types),*> ).into()
}

fn wrap_struct_name( struct_name: &str, input: TokenStream ) -> TokenStream {
    let mut ts = TokenStream::from( Ident::new( struct_name, Span::call_site() ).into_token_stream() );
    ts.extend( Some( TokenTree::Group( Group::new( Delimiter::Brace, input ))));
    ts
}

fn join_fields( fields: impl Iterator<Item=(Ident, Option<Type>)> ) -> (String, Vec<Ident>, Vec<Option<Type>>) {
    let mut fields = fields.collect::<Vec<_>>();
    fields.sort_by_key( |field| field.0.clone() );
    fields
        .into_iter()
        .fold(
            ("structx".to_owned(), Vec::new(), Vec::new() ),
            |(mut struct_name, mut field_idents, mut field_types), (ident, ty)| {
                let field_name = ident.to_string();
                struct_name.push( '_' );
                struct_name.push_str( &field_name.replace( "_", "__" ));
                field_idents.push( ident.clone() );
                field_types.push( ty );
                (struct_name, field_idents, field_types)
            }
        )
}

fn join_field_members<'a>( members: impl Iterator<Item=&'a Member> ) -> (String, Vec<Ident>, Vec<Option<Type>>) {
    join_fields( members
        .map( |member|
            if let Member::Named( ident ) = member {
                ident.clone()
            } else {
                panic!("structx!()'s fields should have names.");
            }
        )
        .zip( (0..).map( |_| None ))
    )
}

struct FnWithNamedArg;

impl VisitMut for FnWithNamedArg {
    fn visit_item_fn_mut( &mut self, item_fn: &mut ItemFn ) {
        visit_mut::visit_item_fn_mut( self, item_fn );

        let inputs = mem::take( &mut item_fn.sig.inputs );
        let fn_args = inputs.into_iter();
        let mut idents = Vec::with_capacity( fn_args.len() );
        let mut types  = Vec::with_capacity( fn_args.len() );

        for fn_arg in fn_args {
            match fn_arg {
                FnArg::Receiver(_) =>
                    item_fn.sig.inputs.push( fn_arg ),
                FnArg::Typed( pat_type ) => {
                    if let Pat::Ident( pat_ident ) = &*pat_type.pat {
                        idents.push( pat_ident.ident.clone() );
                        types .push( pat_type.ty );
                    } else {
                        panic!("#[named_args] function's arguments should be either receiver or `id: Type`.");
                    }
                },
            }
        }
        let (struct_name, field_idents, field_types) =
            join_fields( idents.into_iter().zip( types.into_iter().map( |ty| Some( *ty ))));
        let struct_ident = Ident::new( &struct_name, Span::call_site() );

        item_fn.sig.inputs.push( parse_quote!(
            #struct_ident {
                #( #field_idents ),*
            } : #struct_ident< #(#field_types),*>
        ));
    }
}

/// Simulation of named arguments.
#[proc_macro_attribute]
pub fn named_args( _args: TokenStream, input: TokenStream ) -> TokenStream {
    let mut item_fn = parse_macro_input!( input as ItemFn );
    FnWithNamedArg.visit_item_fn_mut( &mut item_fn );
    quote!( #item_fn ).into()
}

type StructMap = HashMap<String, (Vec<Ident>, Vec<Option<Type>>)>;

#[doc( hidden )]
#[proc_macro]
pub fn scan_structx_from_source_files( input: TokenStream ) -> TokenStream {
    let mut iter = input.into_iter();
    let mut struct_map = StructMap::new();

    loop {
        let token_tree = iter.next();
        match token_tree {
            Some( TokenTree::Literal( literal )) => {
                let file_name = literal.to_string();
                let file_name = file_name.trim_matches('"');
                let contents = String::from_utf8( fs::read( std::path::Path::new( file_name )).unwrap() ).unwrap();
                let syntax = syn::parse_file( &contents ).expect(".rs files should contain valid Rust source code.");
                StructxCollector( &mut struct_map ).visit_file( &syntax );

                if let Some( token_tree ) = iter.next() {
                    if let TokenTree::Punct( punct ) = token_tree {
                        if punct.to_string() != "," {
                            panic!( "scan_structx_from_source_files!(): expect `,`, got `{}`", punct.to_string() );
                        }
                    }
                } else {
                    break;
                }
            },
            None => break,
            _ => panic!( "scan_structx_from_source_files!(): expect string literal, got `{:?}`", token_tree ),
        }
    }

    let mut struct_items = Vec::<ItemStruct>::with_capacity( struct_map.len() );

    for (struct_name, fields) in struct_map {
        let struct_ident = Ident::new( &struct_name, Span::call_site() );
        let generics = ( 0..fields.0.len() )
            .map( |n| Ident::new( &format!( "T{}", n ), Span::call_site() ));
        let field_types = ( 0..fields.1.len() )
            .map( |n| Ident::new( &format!( "T{}", n ), Span::call_site() ));
        let field_idents = fields.0.iter();
        struct_items.push( parse_quote!{
            #[derive( Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash )]
            pub struct #struct_ident <#( #generics ),*> {
                #( pub #field_idents: #field_types,)*
            }
        });
    }

    quote!( #( #struct_items )* ).into()
}

struct StructxCollector<'a>( &'a mut StructMap );

impl<'a> Visit<'_> for StructxCollector<'a> {
    fn visit_macro( &mut self, mac: &Macro ) {
        visit::visit_macro( self, mac );

        if mac.path.leading_colon.is_none() && mac.path.segments.len() == 1 {
            let seg = mac.path.segments.first().unwrap();
            if seg.arguments == PathArguments::None && ( seg.ident == "structx" || seg.ident == "Structx" ) {
                self.parse_structx( mac.tokens.clone().into() );
            }
        }
    }

    fn visit_item_fn( &mut self, item_fn: &ItemFn ) {
        visit::visit_item_fn( self, item_fn );

        for attr in &item_fn.attrs {
            if attr.path.leading_colon.is_none() && attr.path.segments.len() == 1 {
                if attr.path.segments.first().unwrap().ident == "named_args" {
                    let fn_args = item_fn.sig.inputs.iter();
                    let mut idents = Vec::with_capacity( fn_args.len() );
                    let mut types = Vec::with_capacity( fn_args.len() );
                    for fn_arg in fn_args {
                        match fn_arg {
                            FnArg::Receiver(_) => (),
                            FnArg::Typed( pat_type ) => {
                                if let Pat::Ident( pat_ident ) = &*pat_type.pat {
                                    idents.push( pat_ident.ident.clone() );
                                    types .push( (*pat_type.ty).clone() );
                                } else {
                                    panic!("#[named_args] function's arguments should be either receiver or `id: Type`.");
                                }
                            },
                        }
                    }
                    self.add_structx_definition( join_fields( idents.into_iter().zip( types.into_iter().map( Some ))));
                    return;
                }
            }
        }
    }
}

impl<'a> StructxCollector<'a> {
    // parse `structx!{}` in source files.
    fn parse_structx( &mut self, input: TokenStream ) {
        let input_pat = wrap_struct_name( "structx_", input );

        if let Ok( pat ) = syn::parse::<Pat>( input_pat ) {
            if let Pat::Struct( pat_struct ) = pat {
                self.add_structx_definition( join_fields(
                    pat_struct.fields.iter().map( |field| {
                        if let Member::Named( ident ) = &field.member {
                            if let Pat::Type( pat_type ) = &*field.pat {
                                (ident.clone(), Some( (*pat_type.ty).clone() ))
                            } else {
                                (ident.clone(), None )
                            }
                        } else {
                            panic!("structx!()'s fields should have names.");
                        }
                    })
                ));
            } else {
                panic!("structx!()'s supported pattern matching is struct only.");
            }
        }
    }

    fn add_structx_definition( &mut self, (struct_name,field_idents,field_types) : (String, Vec<Ident>, Vec<Option<Type>> )) {
        self.0.entry( struct_name ).or_insert(( field_idents, field_types ));
    }
}
