use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

use quote::ToTokens;

use std::{collections::HashMap, env, fs, path::PathBuf};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    visit::{self, Visit},
    Attribute, Error, Expr, ExprStruct, Field, FnArg, ItemFn, ItemStruct, Macro, Member, Meta, Pat,
    PatStruct, Type, Visibility,
};

/*
 * Introduced a new type which can be parsed with syn::parse2.
 * This is necessary because syn version 2 doesn't implement Parse for Pat
 */
struct PatStructX(PatStruct);

impl Parse for PatStructX {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner = Pat::parse_single(input)?;
        if let Pat::Struct(pat_struct) = inner {
            return Ok(PatStructX(pat_struct));
        }
        Err(Error::new(
            inner.span(),
            "Unsupported pattern in structx macro!",
        ))
    }
}

// A new type which abstracts over a field value type
enum FieldValue {
    Expr(Expr),
    Pat(Pat),
    Type(Type),
}

// StructX is now responsible for parsing the inner part of
// a structx macro.
enum StructX {
    Expr(ExprStruct),
    Item(ItemStruct),
    Pattern(PatStruct),
}

impl StructX {
    const STRUCT_NAME: &'static str = "StructX";

    #[inline]
    fn has_vis(vis: &Visibility) -> bool {
        match vis {
            Visibility::Public(_) => true,
            Visibility::Restricted(_) => true,
            Visibility::Inherited => false,
        }
    }

    #[inline]
    fn field_has_vis(field: &Field) -> bool {
        Self::has_vis(&field.vis)
    }

    fn check_attrs(span: Span, attrs: &Vec<Attribute>) -> syn::Result<()> {
        if !attrs.is_empty() {
            return Err(Error::new(span, "Structx fields can't contain attributes!"));
        }
        Ok(())
    }

    fn check_named(span: Span, member: &Member) -> syn::Result<()> {
        if let Member::Unnamed(_) = member {
            return Err(Error::new(span, "Structx can't contain unnamed fields!"));
        }
        Ok(())
    }

    fn check_item_struct(item_struct: &ItemStruct) -> syn::Result<()> {
        // Because we wrap a struct name around the inner part of the macro
        // the struct shouldn't contain any attributes, generics, visibility
        assert_eq!(item_struct.attrs.len(), 0);
        assert_eq!(item_struct.generics.params.len(), 0);
        assert!(!Self::has_vis(&item_struct.vis));
        for field in &item_struct.fields {
            if field.ident.is_none() {
                return Err(Error::new(field.span(), "Structx fields must have names!"));
            }
            Self::check_attrs(field.span(), &field.attrs)?;
            if Self::field_has_vis(field) {
                return Err(Error::new(
                    field.span(),
                    "Structx fields can't contain visibility modifiers!",
                ));
            }
        }
        Ok(())
    }

    fn check_expr_struct(expr_struct: &ExprStruct) -> syn::Result<()> {
        // Because we wrap a struct name around the inner part of the macro
        // the struct shouldn't contain any attributes, Self type in path,
        // generics in path, leading colon
        assert_eq!(expr_struct.attrs.len(), 0);
        assert!(expr_struct.qself.is_none());
        assert!(expr_struct.path.leading_colon.is_none());
        assert!(expr_struct
            .path
            .segments
            .iter()
            .all(|s| s.arguments.is_none()));
        for field in expr_struct.fields.iter() {
            Self::check_named(field.span(), &field.member)?;
            Self::check_attrs(field.span(), &field.attrs)?;
        }
        Ok(())
    }

    fn check_pat_struct(pat_struct: &PatStruct) -> syn::Result<()> {
        assert_eq!(pat_struct.attrs.len(), 0);
        assert!(pat_struct.qself.is_none());
        assert!(pat_struct.path.leading_colon.is_none());
        assert!(pat_struct
            .path
            .segments
            .iter()
            .all(|s| s.arguments.is_none()));
        for field in pat_struct.fields.iter() {
            Self::check_named(field.span(), &field.member)?;
            Self::check_attrs(field.span(), &field.attrs)?;
        }
        Ok(())
    }

    fn parse_any(input: TokenStream) -> syn::Result<Self> {
        let wrapped_type_input = wrap_struct_name(Self::STRUCT_NAME, input.clone(), true);
        if let Ok(item_struct) = syn::parse2::<ItemStruct>(wrapped_type_input) {
            Self::check_item_struct(&item_struct)?;
            return Ok(StructX::Item(item_struct));
        }
        let wrapped_input = wrap_struct_name(Self::STRUCT_NAME, input, false);
        if let Ok(expr_struct) = syn::parse2::<ExprStruct>(wrapped_input.clone()) {
            Self::check_expr_struct(&expr_struct)?;
            return Ok(StructX::Expr(expr_struct));
        }
        let pat_struct_x = syn::parse2::<PatStructX>(wrapped_input)?;
        Self::check_pat_struct(&pat_struct_x.0)?;
        Ok(StructX::Pattern(pat_struct_x.0))
    }

    fn calc_fields(&self) -> Vec<(Ident, FieldValue)> {
        match self {
            StructX::Expr(expr_struct) => expr_struct
                .fields
                .iter()
                .map(|f| {
                    (
                        named_member_ident(&f.member),
                        FieldValue::Expr(f.expr.clone()),
                    )
                })
                .collect(),
            StructX::Item(item_structs) => item_structs
                .fields
                .iter()
                .map(|f| (f.ident.clone().unwrap(), FieldValue::Type(f.ty.clone())))
                .collect(),
            StructX::Pattern(pat_struct) => pat_struct
                .fields
                .iter()
                .map(|f| {
                    (
                        named_member_ident(&f.member),
                        FieldValue::Pat((*f.pat).clone()),
                    )
                })
                .collect(),
        }
    }
}

#[inline]
fn named_member_ident(member: &Member) -> Ident {
    match member {
        Member::Named(ident) => ident.clone(),
        Member::Unnamed(_) => panic!("Tried to access unnamed member as named member!"),
    }
}

fn wrap_struct_name(
    struct_name: &str,
    input: TokenStream,
    add_struct_keyword: bool,
) -> TokenStream {
    static STRUCT: &'static str = "struct";
    let mut ts = TokenStream::new();
    if add_struct_keyword {
        ts.extend(Ident::new(STRUCT, Span::call_site()).into_token_stream());
    }
    ts.extend(Ident::new(struct_name, Span::call_site()).into_token_stream());
    ts.extend(Some(TokenTree::Group(Group::new(Delimiter::Brace, input))));
    ts
}

fn join_fields(fields: impl Iterator<Item = Ident>) -> (String, Vec<Ident>) {
    static STRUCT_PREFIX: &'static str = "structx";
    let mut fields = fields.collect::<Vec<_>>();
    fields.sort_by_key(|field| field.clone());
    fields.into_iter().fold(
        (STRUCT_PREFIX.to_owned(), Vec::new()),
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

struct StructXCollector<'a>(&'a mut StructMap);

impl<'a> Visit<'_> for StructXCollector<'a> {
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
        self.collect_structx_in_macro(mac);
    }
}

impl<'a> StructXCollector<'a> {
    fn collect_structx_in_macro(&mut self, mac: &Macro) {
        static TYPE_MACRO_STR: &'static str = "Structx";
        static MACRO_STR: &'static str = "structx";
        // TODO support full qualified paths to structx: e.g: structx::structx { ... }
        if mac.path.leading_colon.is_none() && mac.path.segments.len() == 1 {
            let seg = mac.path.segments.first().unwrap();
            if (seg.ident == MACRO_STR || seg.ident == TYPE_MACRO_STR) && seg.arguments.is_none() {
                self.parse_structx(mac.tokens.clone().into());
            }
        }
    }
    // parse `structx!{}`/`Structx!{}`/`args!{}` in source files.
    fn parse_structx(&mut self, input: TokenStream) {
        // Moved parsing logic into StructX
        // StructX::parse_any tries to wrap the inner part of the macro into
        // struct StructX { #inner } and StructX { #inner } and tries to parse it as
        // ItemStruct, ExprStruct and PatStruct
        let struct_x = StructX::parse_any(input).unwrap(); // Throw error if parsing fails
                                                           // Get the field-names + field-values from the parsed struct
        let (fields, values): (Vec<Ident>, Vec<FieldValue>) =
            struct_x.calc_fields().into_iter().unzip();
        // Look for nested structx macro invocations in every field value
        for value in values {
            match value {
                FieldValue::Expr(expr) => {
                    self.visit_expr(&expr);
                }
                FieldValue::Pat(pat) => {
                    self.visit_pat(&pat);
                }
                FieldValue::Type(ty) => {
                    self.visit_type(&ty);
                }
            }
        }
        // Add the struct_name and field_names to the struct map
        let joined_fields = join_fields(fields.into_iter());
        self.add_structx_definition(joined_fields);
    }

    fn add_structx_definition(&mut self, (struct_name, field_idents): (String, Vec<Ident>)) {
        self.0.entry(struct_name).or_insert(field_idents);
    }
}

fn main() {
    let mut struct_map = StructMap::new();
    let mut struct_x_collector = StructXCollector(&mut struct_map);

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
                struct_x_collector.visit_file(&syntax);
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
