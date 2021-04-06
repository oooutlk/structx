use inwelling::*;

use proc_macro2::{
    Delimiter,
    Group,
    Ident,
    Span,
    TokenStream,
    TokenTree,
};

use quote::{
    ToTokens,
};

use std::{
    collections::HashMap,
    env,
    fs,
    path::PathBuf,
};

use syn::{
    FnArg,
    ItemFn,
    Macro,
    Member,
    Pat,
    PathArguments,
    Type,
    visit::{self, Visit},
};

fn wrap_struct_name( struct_name: &str, input: TokenStream ) -> TokenStream {
    let mut ts = Ident::new( struct_name, Span::call_site() ).into_token_stream();
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

type StructMap = HashMap<String, (Vec<Ident>, Vec<Option<Type>>)>;

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

        if let Ok( pat ) = syn::parse2::<Pat>( input_pat ) {
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

fn main() {
    let mut struct_map = StructMap::new();
    let mut structx_collector = StructxCollector( &mut struct_map );

    inwelling( Opts{ watch_manifest: true, watch_rs_files: true, dump_rs_paths: true })
        .sections
        .into_iter()
        .for_each( |section| section.rs_paths.unwrap().into_iter()
            .for_each( |rs_path| {
                let contents = String::from_utf8( fs::read( rs_path ).unwrap() ).unwrap();
                let syntax = syn::parse_file( &contents ).expect(".rs files should contain valid Rust source code.");
                structx_collector.visit_file( &syntax );
            })
        );

    let (lens_traits, optic) = if cfg!( feature = "lens-rs" ) {
        ("#[derive( lens_rs::Optic, lens_rs::Lens )]", "#[optic] ")
    } else {
        ("", "")
    };

    let output = struct_map
        .into_iter()
        .fold( String::new(), |acc, (struct_name, (field_idents,field_types))| {
            format!( r#"{}
#[allow( non_camel_case_types )]
{}
#[derive( Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash )]
pub struct {}<{}>{{{}
}}
"#,
                    acc,
                    lens_traits,
                    struct_name,
                    ( 1..field_types.len() )
                        .fold( "T0".to_owned(), |acc, nth| format!( "{},T{}", acc, nth )),
                    field_idents
                        .into_iter()
                        .enumerate()
                        .fold( String::new(), |acc, (nth, field)| format!( "{}\n    {}pub {}: T{},", acc, optic, field, nth ))
            )});

    let out_path = PathBuf::from( env::var( "OUT_DIR" ).expect( "$OUT_DIR should exist." ));
    std::fs::write( out_path.join( "bindings.rs" ), output ).expect( "bindings.rs generated." );
}
