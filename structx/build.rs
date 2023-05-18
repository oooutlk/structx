use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream, TokenTree};

use quote::ToTokens;

use std::{collections::HashMap, env, fs, path::PathBuf};
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{
    visit::{self, Visit},
    Arm, Attribute, Block, Error, Expr, ExprStruct, Field, FnArg, GenericArgument, ItemFn,
    ItemStruct, Macro, Member, Meta, Pat, PatStruct, Path, PathArguments, ReturnType, Stmt, Type,
    Visibility,
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

//
// Introduced a new type which abstracts over a field value type
//
enum FieldValue {
    Expr(Expr),
    Pat(Pat),
    Type(Type),
}

//
// StructX is now responsible for parsing the inner part of
// a structx macro.
//
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
    fn collect_structx_in_arm(&mut self, arm: &Arm) {
        self.collect_structx_in_pat(&arm.pat);
        self.collect_structx_in_expr(&arm.body);
        arm.guard
            .iter()
            .for_each(|(_, guard)| self.collect_structx_in_expr(guard));
    }
    fn collect_structx_in_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Local(_) => {}
            Stmt::Item(_) => {}
            Stmt::Expr(_, _) => {}
            Stmt::Macro(_) => {}
        }
    }
    /*
     * Can occur when we use nested structx!() in const generics e.g:
     * structx! {
     *  member: MyStruct::<structx!{ name: 0}> { .. }
     * }
     */
    fn collect_structx_in_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Array(array_expr) => {
                array_expr
                    .elems
                    .iter()
                    .for_each(|inner| self.collect_structx_in_expr(inner));
            }
            Expr::Assign(assign_expr) => {
                self.collect_structx_in_expr(&assign_expr.left);
                self.collect_structx_in_expr(&assign_expr.right);
            }
            Expr::Async(async_expr) => self.collect_structx_in_block(&async_expr.block),

            Expr::Await(await_expr) => self.collect_structx_in_expr(&await_expr.base),
            Expr::Binary(binary_expr) => {
                self.collect_structx_in_expr(&binary_expr.left);
                self.collect_structx_in_expr(&binary_expr.right);
            }
            Expr::Block(block_expr) => self.collect_structx_in_block(&block_expr.block),
            Expr::Break(break_expr) => break_expr
                .expr
                .iter()
                .for_each(|inner| self.collect_structx_in_expr(inner)),
            Expr::Call(call_expr) => {
                call_expr
                    .args
                    .iter()
                    .for_each(|arg| self.collect_structx_in_expr(arg));
                self.collect_structx_in_expr(&call_expr.func);
            }
            Expr::Cast(cast) => self.collect_structx_in_expr(&cast.expr),
            Expr::Closure(closure_expr) => {
                self.collect_structx_in_return_type(&closure_expr.output);
                closure_expr
                    .inputs
                    .iter()
                    .for_each(|arg_pat| self.collect_structx_in_pat(arg_pat));
                self.collect_structx_in_expr(&closure_expr.body);
            }
            Expr::Const(const_expr) => self.collect_structx_in_block(&const_expr.block),
            Expr::Continue(_) => {}
            Expr::Field(field) => self.collect_structx_in_expr(&field.base),
            Expr::ForLoop(for_loop_expr) => {
                self.collect_structx_in_pat(&for_loop_expr.pat);
                self.collect_structx_in_expr(&for_loop_expr.expr);
                self.collect_structx_in_block(&for_loop_expr.body);
            }
            Expr::Group(group) => self.collect_structx_in_expr(&group.expr),
            Expr::If(if_expr) => {
                self.collect_structx_in_expr(&if_expr.cond);
                self.collect_structx_in_block(&if_expr.then_branch);
                if_expr
                    .else_branch
                    .iter()
                    .for_each(|(_, inner)| self.collect_structx_in_expr(&inner));
            }
            Expr::Index(index_expr) => {
                self.collect_structx_in_expr(&index_expr.expr);
                self.collect_structx_in_expr(&index_expr.index);
            }
            Expr::Infer(_) => {}
            Expr::Let(let_expr) => {
                self.collect_structx_in_expr(&let_expr.expr);
                self.collect_structx_in_pat(&let_expr.pat);
            }
            Expr::Lit(_) => {}
            Expr::Loop(loop_expr) => self.collect_structx_in_block(&loop_expr.body),
            Expr::Macro(mac_expr) => self.collect_structx_in_macro(&mac_expr.mac),
            Expr::Match(match_expr) => {
                self.collect_structx_in_expr(&match_expr.expr);
                match_expr
                    .arms
                    .iter()
                    .for_each(|arm| self.collect_structx_in_arm(arm));
            }
            Expr::MethodCall(method_call_expr) => {
                method_call_expr
                    .args
                    .iter()
                    .for_each(|arg| self.collect_structx_in_expr(arg));
                self.collect_structx_in_expr(&method_call_expr.receiver);
                method_call_expr.turbofish.iter().for_each(|abg| {
                    abg.args
                        .iter()
                        .for_each(|ga| self.collect_structx_in_generic_argument(ga))
                });
            }
            Expr::Paren(paren_expr) => self.collect_structx_in_expr(&paren_expr.expr),
            Expr::Path(path_expr) => self.collect_structx_in_path(&path_expr.path),
            Expr::Range(range_expr) => {
                range_expr
                    .start
                    .iter()
                    .for_each(|start| self.collect_structx_in_expr(&start));
                range_expr
                    .end
                    .iter()
                    .for_each(|end| self.collect_structx_in_expr(&end));
            }
            Expr::Reference(ref_expr) => self.collect_structx_in_expr(&ref_expr.expr),
            Expr::Repeat(repeat_expr) => {
                self.collect_structx_in_expr(&repeat_expr.expr);
                self.collect_structx_in_expr(&repeat_expr.len);
            }
            Expr::Return(return_expr) => return_expr
                .expr
                .iter()
                .for_each(|expr| self.collect_structx_in_expr(expr)),
            Expr::Struct(struct_expr) => {
                self.collect_structx_in_path(&struct_expr.path);
                struct_expr
                    .fields
                    .iter()
                    .for_each(|field| self.collect_structx_in_expr(&field.expr));
                struct_expr
                    .rest
                    .iter()
                    .for_each(|rest| self.collect_structx_in_expr(rest));
            }
            Expr::Try(try_expr) => self.collect_structx_in_expr(&try_expr.expr),
            Expr::TryBlock(try_block_expr) => self.collect_structx_in_block(&try_block_expr.block),
            Expr::Tuple(tuple_expr) => tuple_expr
                .elems
                .iter()
                .for_each(|expr| self.collect_structx_in_expr(expr)),
            Expr::Unary(unary_expr) => self.collect_structx_in_expr(&unary_expr.expr),
            Expr::Unsafe(unsafe_expr) => self.collect_structx_in_block(&unsafe_expr.block),
            Expr::Verbatim(_) => {}
            Expr::While(while_expr) => {
                self.collect_structx_in_expr(&while_expr.cond);
                self.collect_structx_in_block(&while_expr.body);
            }
            Expr::Yield(yield_expr) => yield_expr
                .expr
                .iter()
                .for_each(|expr| self.collect_structx_in_expr(&expr)),
            _ => {}
        }
    }
    fn collect_structx_in_generic_argument(&mut self, arg: &GenericArgument) {
        match arg {
            GenericArgument::Type(typ) => self.collect_structx_in_type(typ),
            GenericArgument::Const(e) => self.collect_structx_in_expr(e),
            GenericArgument::AssocType(at) => {
                at.generics.iter().for_each(|gat| {
                    gat.args
                        .iter()
                        .for_each(|arg| self.collect_structx_in_generic_argument(arg))
                });
                self.collect_structx_in_type(&at.ty);
            }
            GenericArgument::AssocConst(ac) => {
                ac.generics.iter().for_each(|gat| {
                    gat.args
                        .iter()
                        .for_each(|arg| self.collect_structx_in_generic_argument(arg))
                });
                self.collect_structx_in_expr(&ac.value);
            }
            GenericArgument::Constraint(_) | GenericArgument::Lifetime(_) => {}
            _ => {}
        }
    }
    fn collect_structx_in_path_arguments(&mut self, segment: &PathArguments) {
        match segment {
            PathArguments::AngleBracketed(generics) => {
                generics
                    .args
                    .iter()
                    .for_each(|arg| self.collect_structx_in_generic_argument(arg));
            }
            PathArguments::Parenthesized(args) => {
                args.inputs
                    .iter()
                    .for_each(|arg| self.collect_structx_in_type(arg));
                self.collect_structx_in_return_type(&args.output);
            }
            PathArguments::None => {}
        }
    }
    fn collect_structx_in_block(&mut self, block: &Block) {
        block
            .stmts
            .iter()
            .for_each(|stmt| self.collect_structx_in_stmt(stmt));
    }
    fn collect_structx_in_path(&mut self, path: &Path) {
        path.segments
            .iter()
            .for_each(|seg| self.collect_structx_in_path_arguments(&seg.arguments));
    }
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
        // TODO add nested anonymous structs in macros such as vec![]
    }
    fn collect_structx_in_return_type(&mut self, ret_type: &ReturnType) {
        match &ret_type {
            ReturnType::Default => {}
            ReturnType::Type(_, typ) => self.collect_structx_in_type(&typ),
        };
    }
    fn collect_structx_in_type(&mut self, typ: &Type) {
        match typ {
            Type::Array(arr) => self.collect_structx_in_type(&arr.elem),
            Type::BareFn(f) => {
                f.inputs
                    .iter()
                    .for_each(|arg| self.collect_structx_in_type(&arg.ty));
                self.collect_structx_in_return_type(&f.output);
            }
            Type::Group(g) => self.collect_structx_in_type(&g.elem),
            Type::Macro(m) => self.collect_structx_in_macro(&m.mac),
            Type::Paren(p) => self.collect_structx_in_type(&p.elem),
            Type::Path(p) => {
                self.collect_structx_in_path(&p.path);
            }
            Type::Ptr(p) => self.collect_structx_in_type(&p.elem),
            Type::Reference(r) => self.collect_structx_in_type(&r.elem),
            Type::Slice(s) => self.collect_structx_in_type(&s.elem),
            Type::Tuple(tt) => {
                tt.elems
                    .iter()
                    .for_each(|typ| self.collect_structx_in_type(typ));
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
    fn collect_structx_in_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Macro(mp) => {
                self.collect_structx_in_macro(&mp.mac);
            }
            Pat::Or(op) => {
                op.cases.iter().for_each(|pat| {
                    self.collect_structx_in_pat(&pat);
                });
            }
            Pat::Path(pp) => {
                self.collect_structx_in_path(&pp.path);
            }
            Pat::Range(rp) => {
                rp.start
                    .iter()
                    .for_each(|s| self.collect_structx_in_expr(&s));
                rp.end.iter().for_each(|e| self.collect_structx_in_expr(&e));
            }
            Pat::Reference(rp) => {
                self.collect_structx_in_pat(&rp.pat);
            }
            Pat::Slice(sp) => {
                sp.elems.iter().for_each(|pat| {
                    self.collect_structx_in_pat(pat);
                });
            }
            Pat::Struct(sp) => {
                sp.fields.iter().for_each(|pat| {
                    self.collect_structx_in_pat(&pat.pat);
                });
            }
            Pat::Tuple(tp) => {
                tp.elems.iter().for_each(|pat| {
                    self.collect_structx_in_pat(pat);
                });
            }
            Pat::TupleStruct(ts) => {
                ts.elems.iter().for_each(|pat| {
                    self.collect_structx_in_pat(pat);
                });
                self.collect_structx_in_path(&ts.path);
            }
            Pat::Type(pat_type) => {
                self.collect_structx_in_type(&pat_type.ty);
            }
            Pat::Ident(ip) => {
                ip.subpat.iter().for_each(|(_, pat)| {
                    self.collect_structx_in_pat(&pat);
                });
            }
            Pat::Rest(_) | Pat::Lit(_) => {}
            Pat::Verbatim(_) | Pat::Wild(_) | _ => {
                panic!("Nested pattern {:?} not supported by structx()!", pat);
            }
        };
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
                FieldValue::Expr(expr) => self.collect_structx_in_expr(&expr),
                FieldValue::Pat(pat) => self.collect_structx_in_pat(&pat),
                FieldValue::Type(ty) => self.collect_structx_in_type(&ty),
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
