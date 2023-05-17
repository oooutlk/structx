use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

use quote::ToTokens;

use std::{collections::HashMap, env, fs, path::PathBuf};

use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    visit::{self, Visit},
    Error, Expr, FnArg, GenericArgument, ItemFn, Macro, Member, Meta, Pat, PatStruct, Path,
    PathArguments, ReturnType, Type,
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
            Err(Error::new(
                pat.span(),
                "structx!()'s supported pattern matching is struct only.",
            ))
        }
    }
}

fn wrap_struct_name(struct_name: &str, input: TokenStream) -> TokenStream {
    let mut ts = Ident::new(struct_name, Span::call_site()).into_token_stream();
    ts.extend(Some(TokenTree::Group(Group::new(Delimiter::Brace, input))));
    ts
}

fn join_fields(fields: impl Iterator<Item = Ident>) -> (String, Vec<Ident>) {
    let mut fields = fields.collect::<Vec<_>>();
    fields.sort_by_key(|field| field.clone());
    fields.into_iter().fold(
        ("structx".to_owned(), Vec::new()),
        |(mut struct_name, mut field_idents), ident| {
            let field_name = ident.to_string();
            struct_name.push('_');
            struct_name.push_str(&field_name.replace("_", "__"));
            field_idents.push(ident);
            (struct_name, field_idents)
        },
    )
}

type StructMap = HashMap<String, Vec<Ident>>;

struct StructxCollector<'a>(&'a mut StructMap);

impl<'a> Visit<'_> for StructxCollector<'a> {
    fn visit_item_fn(&mut self, item_fn: &ItemFn) {
        visit::visit_item_fn(self, item_fn);

        for attr in &item_fn.attrs {
            if let Meta::Path(path) = &attr.meta {
                if path.leading_colon.is_none() && path.segments.len() == 1 {
                    if path.segments.first().unwrap().ident == "named_args" {
                        let fn_args = item_fn.sig.inputs.iter();
                        let mut idents = Vec::with_capacity(fn_args.len());
                        let mut types = Vec::with_capacity(fn_args.len());
                        for fn_arg in fn_args {
                            match fn_arg {
                                FnArg::Receiver(_) => (),
                                FnArg::Typed(pat_type) => {
                                    if let Pat::Ident(pat_ident) = &*pat_type.pat {
                                        idents.push(pat_ident.ident.clone());
                                        types.push((*pat_type.ty).clone());
                                    } else {
                                        panic!("#[named_args] function's arguments should be either receiver or `id: Type`.");
                                    }
                                }
                            }
                        }
                        self.add_structx_definition(join_fields(idents.into_iter()));
                        return;
                    }
                }
            }
        }
    }

    fn visit_macro(&mut self, mac: &Macro) {
        visit::visit_macro(self, mac);
        self.parse_structx_in_macro(mac);
    }
}

impl<'a> StructxCollector<'a> {
    /*
     * Can occur when we use nested structx!() in const generics e.g:
     * structx! {
     *  member: MyStruct::<structx!{ name: 0}> { .. }
     * }
     */
    fn parse_structx_in_expr(&mut self, _: &Expr) {
        // TODO support in the future
        panic!("nested expressions in structx!() not supported yet!");
    }
    fn parse_structx_in_generic_argument(&mut self, arg: &GenericArgument) {
        match arg {
            GenericArgument::Type(typ) => self.parse_structx_in_type(typ),
            GenericArgument::Const(e) => self.parse_structx_in_expr(e),
            GenericArgument::AssocType(at) => {
                at.generics.iter().for_each(|gat| {
                    gat.args
                        .iter()
                        .for_each(|arg| self.parse_structx_in_generic_argument(arg))
                });
                self.parse_structx_in_type(&at.ty);
            }
            GenericArgument::AssocConst(ac) => {
                ac.generics.iter().for_each(|gat| {
                    gat.args
                        .iter()
                        .for_each(|arg| self.parse_structx_in_generic_argument(arg))
                });
                self.parse_structx_in_expr(&ac.value);
            }
            GenericArgument::Constraint(_) | GenericArgument::Lifetime(_) => {}
            _ => {}
        }
    }
    fn parse_structx_in_path_arguments(&mut self, segment: &PathArguments) {
        match segment {
            PathArguments::AngleBracketed(generics) => {
                generics
                    .args
                    .iter()
                    .for_each(|arg| self.parse_structx_in_generic_argument(arg));
            }
            PathArguments::Parenthesized(args) => {
                args.inputs
                    .iter()
                    .for_each(|arg| self.parse_structx_in_type(arg));
                self.parse_structx_in_return_type(&args.output);
            }
            PathArguments::None => {}
        }
    }
    fn parse_structx_in_path(&mut self, path: &Path) {
        path.segments
            .iter()
            .for_each(|seg| self.parse_structx_in_path_arguments(&seg.arguments));
    }
    fn parse_structx_in_macro(&mut self, mac: &Macro) {
        static TYPE_MACRO_STR: &'static str = "Structx";
        static MACRO_STR: &'static str = "structx";
        // TODO support full qualified paths to structx: e.g: structx::structx { ... }
        if mac.path.leading_colon.is_none() && mac.path.segments.len() == 1 {
            let seg = mac.path.segments.first().unwrap();
            if (seg.ident == MACRO_STR || seg.ident == TYPE_MACRO_STR) && seg.arguments.is_none() {
                self.parse_structx(mac.tokens.clone().into());
            }
        }
        // TODO add nested anonymous structs in popular macros such as vec![]
    }
    fn parse_structx_in_return_type(&mut self, ret_type: &ReturnType) {
        match &ret_type {
            ReturnType::Default => {}
            ReturnType::Type(_, typ) => self.parse_structx_in_type(&typ),
        };
    }
    fn parse_structx_in_type(&mut self, typ: &Type) {
        match typ {
            Type::Array(arr) => self.parse_structx_in_type(&arr.elem),
            Type::BareFn(f) => {
                f.inputs
                    .iter()
                    .for_each(|arg| self.parse_structx_in_type(&arg.ty));
                self.parse_structx_in_return_type(&f.output);
            }
            Type::Group(g) => self.parse_structx_in_type(&g.elem),
            Type::Macro(m) => self.parse_structx_in_macro(&m.mac),
            Type::Paren(p) => self.parse_structx_in_type(&p.elem),
            Type::Path(p) => {
                self.parse_structx_in_path(&p.path);
            }
            Type::Ptr(p) => self.parse_structx_in_type(&p.elem),
            Type::Reference(r) => self.parse_structx_in_type(&r.elem),
            Type::Slice(s) => self.parse_structx_in_type(&s.elem),
            Type::Tuple(tt) => {
                tt.elems
                    .iter()
                    .for_each(|typ| self.parse_structx_in_type(typ));
            }
            // These types represent leaf types which can't contain any more anonymous struct types
            Type::Infer(_)
            | Type::Never(_)
            | Type::ImplTrait(_)
            | Type::TraitObject(_)
            | Type::Verbatim(_) => {}
            _ => panic!("Unknown type {:?} in structx()! ", typ),
        }
    }
    fn parse_structx_in_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Macro(mp) => {
                self.parse_structx_in_macro(&mp.mac);
            }
            Pat::Or(op) => {
                op.cases.iter().for_each(|pat| {
                    self.parse_structx_in_pat(&pat);
                });
            }
            Pat::Path(pp) => {
                self.parse_structx_in_path(&pp.path);
            }
            Pat::Range(rp) => {
                rp.start.iter().for_each(|s| self.parse_structx_in_expr(&s));
                rp.end.iter().for_each(|e| self.parse_structx_in_expr(&e));
            }
            Pat::Reference(rp) => {
                self.parse_structx_in_pat(&rp.pat);
            }
            Pat::Slice(sp) => {
                sp.elems.iter().for_each(|pat| {
                    self.parse_structx_in_pat(pat);
                });
            }
            Pat::Struct(sp) => {
                sp.fields.iter().for_each(|pat| {
                    self.parse_structx_in_pat(&pat.pat);
                });
            }
            Pat::Tuple(tp) => {
                tp.elems.iter().for_each(|pat| {
                    self.parse_structx_in_pat(pat);
                });
            }
            Pat::TupleStruct(ts) => {
                ts.elems.iter().for_each(|pat| {
                    self.parse_structx_in_pat(pat);
                });
                self.parse_structx_in_path(&ts.path);
            }
            Pat::Type(pat_type) => {
                self.parse_structx_in_type(&pat_type.ty);
            }
            Pat::Ident(ip) => {
                ip.subpat.iter().for_each(|(_, pat)| {
                    self.parse_structx_in_pat(&pat);
                });
            }
            Pat::Rest(_) | Pat::Lit(_) => {}
            Pat::Verbatim(_) | Pat::Wild(_) | _ => {
                panic!("Nested pattern {:?} not supported by structx()!", pat);
            }
        };
    }
    // parse `structx!{}`/`Structx!{}` in source files.
    fn parse_structx(&mut self, input: TokenStream) {
        static STRUCT_PREFIX_STR: &'static str = "structx_";
        let input_pat = wrap_struct_name(STRUCT_PREFIX_STR, input);
        if let Ok(struct_x) = syn::parse2::<StructX>(input_pat) {
            let joined_fields = join_fields(struct_x.pat.fields.iter().map(|field| {
                if let Member::Named(ident) = &field.member {
                    self.parse_structx_in_pat(&field.pat);
                    ident.clone()
                } else {
                    panic!("structx!()'s fields should have names.");
                }
            }));
            self.add_structx_definition(joined_fields);
        } // Structx!{}'s inner tokens may not be parsed as pattern.
    }

    fn add_structx_definition(&mut self, (struct_name, field_idents): (String, Vec<Ident>)) {
        self.0.entry(struct_name).or_insert(field_idents);
    }
}

fn main() {
    let mut struct_map = StructMap::new();
    let mut structx_collector = StructxCollector(&mut struct_map);

    inwelling::collect_downstream(inwelling::Opts {
        watch_manifest: true,
        watch_rs_files: true,
        dump_rs_paths: true,
    })
    .packages
    .into_iter()
    .for_each(|package| {
        package.rs_paths.unwrap().into_iter().for_each(|rs_path| {
            let contents = String::from_utf8(fs::read(rs_path.clone()).unwrap()).unwrap();
            let syntax = syn::parse_file(&contents);
            if let Ok(syntax) = syntax {
                structx_collector.visit_file(&syntax);
            } // it's better to report compile errors in downstream crates
        })
    });

    let (lens_traits, optic) = if cfg!(feature = "lens-rs") {
        ("#[derive( lens_rs::Lens )]", "#[optic] ")
    } else {
        ("", "")
    };

    let output = struct_map
        .into_iter()
        .fold(String::new(), |acc, (struct_name, field_idents)| {
            format!(
                r#"{}
#[allow( non_camel_case_types )]
{lens_traits}
#[derive( Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash )]
pub struct {struct_name}<{generics}>{{{fields}
}}
"#,
                acc,
                lens_traits = lens_traits,
                struct_name = struct_name,
                generics = (1..field_idents.len())
                    .fold("T0".to_owned(), |acc, nth| format!("{},T{}", acc, nth)),
                fields = field_idents.iter().enumerate().fold(
                    String::new(),
                    |acc, (nth, field)| format!("{}\n    {}pub {}: T{},", acc, optic, field, nth)
                ),
            )
        });

    let out_path = PathBuf::from(env::var("OUT_DIR").expect("$OUT_DIR should exist."));
    fs::write(out_path.join("bindings.rs"), output).expect("bindings.rs generated.");
}
