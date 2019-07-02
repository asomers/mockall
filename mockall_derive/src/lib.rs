// vim: tw=80
//! Proc Macros for use with Mockall
//!
//! You probably don't want to use this crate directly.  Instead, you use use
//! its reexports via the [`mockall`](../mockall/index.html) crate.

#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    Token,
    spanned::Spanned
};

mod automock;
mod mock;
use crate::automock::do_automock;
use crate::mock::{Mock, do_mock};

struct MethodTypes {
    is_static: bool,
    /// Type of Expectation returned by the expect method
    expectation: syn::Type,
    /// Type of Expectations container used by the expect method, without its
    /// generics fields
    expectations: syn::Type,
    /// Type of Expectations object stored in the mock structure, with generics
    /// fields
    expect_obj: syn::Type,
    /// Method to call when invoking the expectation
    call: syn::Ident,
    /// Version of the arguments used for the Predicates
    altargs: syn::punctuated::Punctuated<syn::ArgCaptured, Token![,]>,
    /// Expressions producing references from the arguments
    matchexprs: syn::punctuated::Punctuated<syn::Expr, Token![,]>,
}

cfg_if! {
    // proc-macro2's Span::unstable method requires the nightly feature, and it
    // doesn't work in test mode.
    // https://github.com/alexcrichton/proc-macro2/issues/159
    if #[cfg(all(feature = "nightly", not(test)))] {
        fn compile_error(span: Span, msg: &'static str) {
            span.unstable()
                .error(msg)
                .emit();
        }
    } else {
        fn compile_error(_span: Span, msg: &str) {
            panic!("{}.  More information may be available when mockall is built with the \"nightly\" feature.", msg);
        }
    }
}

/// Replace any "impl trait" types with "Box<dyn trait>" equivalents
fn deimplify(ty: &mut syn::Type) {
    if let syn::Type::ImplTrait(tit) = ty {
        let bounds = &tit.bounds;
        *ty = syn::parse2(quote!(Box<dyn #bounds>)).unwrap();
    }
}

/// Remove any mutability qualifiers from a method's argument list
fn demutify(args: &syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>)
    -> syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>
{
    let mut output = args.clone();
    for arg in output.iter_mut() {
        match arg {
            syn::FnArg::SelfValue(arg_self) => arg_self.mutability = None,
            syn::FnArg::Captured(arg_captured) => demutify_arg(arg_captured),
            _ => (),    // Nothing to do
        }
    }
    output
}

/// Remove any "mut" from a method argument's binding.
fn demutify_arg(arg: &mut syn::ArgCaptured) {
    match arg.pat {
        syn::Pat::Wild(_) => {
            compile_error(arg.span(),
                "Mocked methods must have named arguments");
        },
        syn::Pat::Ident(ref mut pat_ident) => {
            if let Some(r) = &pat_ident.by_ref {
                compile_error(r.span(),
                    "Mockall does not support by-reference argument bindings");
            }
            if let Some((_at, subpat)) = &pat_ident.subpat {
                compile_error(subpat.span(),
                    "Mockall does not support subpattern bindings");
            }
            pat_ident.mutability = None;
        },
        _ => {
            compile_error(arg.span(), "Unsupported argument type");
        }
    };
}

/// Replace any references to `Self` in `literal_type` with `actual`.  Useful
/// for constructor methods
fn deselfify(literal_type: &mut syn::Type, actual: &syn::Ident) {
    match literal_type {
        syn::Type::Slice(s) => {
            deselfify(s.elem.as_mut(), actual);
        },
        syn::Type::Array(a) => {
            deselfify(a.elem.as_mut(), actual);
        },
        syn::Type::Ptr(p) => {
            deselfify(p.elem.as_mut(), actual);
        },
        syn::Type::Reference(r) => {
            deselfify(r.elem.as_mut(), actual);
        },
        syn::Type::BareFn(_bfn) => {
            unimplemented!()
        },
        syn::Type::Tuple(tuple) => {
            for elem in tuple.elems.iter_mut() {
                deselfify(elem, actual);
            }
        }
        syn::Type::Path(type_path) => {
            if let Some(ref _qself) = type_path.qself {
                compile_error(type_path.span(), "QSelf is TODO");
            }
            let p = &mut type_path.path;
            for seg in p.segments.iter_mut() {
                if seg.ident == "Self" {
                    seg.ident = actual.clone()
                }
                if let syn::PathArguments::AngleBracketed(abga) =
                    &mut seg.arguments
                {
                    for arg in abga.args.iter_mut() {
                        match arg {
                            syn::GenericArgument::Type(ty) =>
                                deselfify(ty, actual),
                            syn::GenericArgument::Binding(b) =>
                                deselfify(&mut b.ty, actual),
                            _ => /* Nothing to do */(),
                        }
                    }
                }
            }
        },
        syn::Type::Paren(p) => {
            deselfify(p.elem.as_mut(), actual);
        },
        syn::Type::Group(g) => {
            deselfify(g.elem.as_mut(), actual);
        },
        syn::Type::Macro(_) | syn::Type::Verbatim(_) => {
            compile_error(literal_type.span(),
                "mockall_derive does not support this type as a return argument");
        },
        syn::Type::ImplTrait(_) => {
            panic!("deimplify should've already been run on this output type");
        },
        syn::Type::TraitObject(tto) => {
            // Change types like `dyn Self` into `MockXXX`.  For now,
            // don't worry about multiple trait bounds, because they aren't very
            // useful in combination with Self.
            if tto.bounds.len() == 1 {
                if let syn::TypeParamBound::Trait(t)
                    = tto.bounds.first().unwrap().value()
                {
                    let path = &t.path;
                    let mut new_type: syn::Type
                        = syn::parse2(quote!(#path)).unwrap();
                    deselfify(&mut new_type, actual);
                    *literal_type = new_type;
                }
            }
        },
        syn::Type::Infer(_) | syn::Type::Never(_) =>
        {
            /* Nothing to do */
        }
    }
}

/// Generate a mock identifier from the regular one: eg "Foo" => "MockFoo"
fn gen_mock_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("Mock{}", ident), ident.span())
}

/// Generate an identifier for the mock struct's private module: eg "Foo" =>
/// "__mock_Foo"
fn gen_mod_ident(struct_: &syn::Ident, trait_: Option<&syn::Ident>)
    -> syn::Ident
{
    if let Some(t) = trait_ {
        syn::Ident::new(&format!("__mock_{}_{}", struct_, t), struct_.span())
    } else {
        syn::Ident::new(&format!("__mock_{}", struct_), struct_.span())
    }
}

/// Combine two Generics structs, producing a new one that has the union of
/// their parameters.  The output's parameter bounds are not accurate!
fn merge_generics(x: &syn::Generics, y: &syn::Generics) -> syn::Generics {
    /// Compare only the identifiers of two GenericParams
    fn cmp_gp_idents(x: &syn::GenericParam, y: &syn::GenericParam) -> bool {
        use syn::GenericParam::*;

        match (x, y) {
            (Type(xtp), Type(ytp)) => {
                xtp.ident == ytp.ident
            },
            (Lifetime(xld), Lifetime(yld)) => {
                xld.lifetime == yld.lifetime
            },
            (Const(xc), Const(yc)) => {
                xc.ident == yc.ident
            },
            _ => false
        }
    }

    /// Compare only the identifiers of two WherePredicates
    fn cmp_wp_idents(x: &syn::WherePredicate, y: &syn::WherePredicate) -> bool {
        use syn::WherePredicate::*;

        match (x, y) {
            (Type(xpt), Type(ypt)) => {
                xpt.bounded_ty == ypt.bounded_ty
            },
            (Lifetime(xpl), Lifetime(ypl)) => {
                xpl.lifetime == ypl.lifetime
            },
            (Eq(xeq), Eq(yeq)) => {
                xeq.lhs_ty == yeq.lhs_ty
            },
            _ => false
        }
    }

    if x.lt_token.is_none() {
        y.clone()
    } else if y.lt_token.is_none() {
        x.clone()
    } else {
        let mut out = x.clone();
        // First merge the params
        for param in y.params.iter() {
            if ! x.params.iter().any(|t| cmp_gp_idents(t, param)) {
                out.params.push(param.clone());
            }
        }
        // Then merge the where clauses
        if x.where_clause.is_none() {
            out.where_clause = y.where_clause.clone();
        } else if let Some(wc) = &y.where_clause {
            if x.where_clause.is_none() {
                out.where_clause = Some(wc.clone());
            }
            for predicate in wc.predicates.iter() {
                let x_p = &x.where_clause.as_ref().unwrap().predicates;
                let out_p = &mut out.where_clause.as_mut().unwrap().predicates;
                if ! x_p.iter().any(|t| cmp_wp_idents(t, predicate)) {
                    out_p.push(predicate.clone());
                }
            }
        }
        out
    }
}

/// Return the visibility that should be used for expectation!, given the
/// original method's visibility.
///
/// # Arguments
/// - `vis`:    Original visibility of the item
/// - `levels`: How many modules will the mock item be nested in?
fn expectation_visibility(vis: &syn::Visibility, levels: u32)
    -> syn::Visibility
{
    debug_assert!(levels > 0);
    let in_token = Token![in](vis.span());
    let super_token = Token![super](vis.span());
    match vis {
        syn::Visibility::Inherited => {
            // Private items need pub(in super::[...]) for each level
            let mut path = syn::Path::from(super_token);
            for _ in 1..levels {
                path.segments.push(super_token.into());
            }
            let supersuper = syn::Visibility::Restricted(syn::VisRestricted{
                pub_token: Token![pub](vis.span()),
                paren_token: syn::token::Paren::default(),
                in_token: Some(in_token),
                path: Box::new(path)
            });
            supersuper.into()
        },
        syn::Visibility::Restricted(vr) => {
            // crate => don't change
            // in crate::* => don't change
            // super => in super::super::super
            // self => in super::super
            // in anything_else => super::super::anything_else
            if vr.path.segments.first().unwrap().value().ident == "crate" {
                vr.clone().into()
            } else {
                let mut out = vr.clone();
                out.in_token = Some(in_token);
                for _ in 0..levels {
                    out.path.segments.insert(0, super_token.into());
                }
                out.into()
            }
        },
        _ => vis.clone()
    }
}

/// Extract useful data about a method
///
/// # Arguments
///
/// * `sig`:            Signature of the original method
/// * `generics`:       Generics of the method's parent trait or structure,
///                     _not_ the method itself.
fn method_types(sig: &syn::MethodSig, generics: Option<&syn::Generics>)
    -> MethodTypes
{
    let mut is_static = true;
    let mut altargs = syn::punctuated::Punctuated::new();
    let mut matchexprs = syn::punctuated::Punctuated::new();
    let mut args
        = syn::punctuated::Punctuated::<syn::Type, Token![,]>::new();
    let ident = &sig.ident;
    let is_generic = !sig.decl.generics.params.is_empty();
    let merged_g = if let Some(g) = generics {
        merge_generics(&g, &sig.decl.generics)
    } else {
        sig.decl.generics.clone()
    };
    let (_ig, tg, _wc) = merged_g.split_for_impl();
    for (i, fn_arg) in sig.decl.inputs.iter().enumerate() {
        match fn_arg {
            syn::FnArg::Captured(arg) => {
                args.push(arg.ty.clone());
                let mep = if let syn::Pat::Ident(arg_ident) = &arg.pat {
                    syn::Expr::Path(syn::ExprPath{
                        attrs: Vec::new(),
                        qself: None,
                        path: syn::Path::from(arg_ident.ident.clone())
                    })
                } else {
                    compile_error(arg.span(),
                        "Unexpected argument pattern in function declaration");
                    break;
                };
                let alt_ty = if let syn::Type::Reference(tr) = &arg.ty {
                    /* Remove any "mut" */
                    let mut alt_tr = tr.clone();
                    alt_tr.mutability = None;
                    matchexprs.push(mep);
                    syn::Type::Reference(alt_tr)
                } else {
                    let matchexpr = syn::Expr::Reference(syn::ExprReference {
                        attrs: Vec::new(),
                        and_token: syn::Token![&](arg.span()),
                        mutability: None,
                        expr: Box::new(mep)
                    });
                    matchexprs.push(matchexpr);
                    syn::Type::Reference(syn::TypeReference{
                        and_token: syn::Token![&](arg.span()),
                        lifetime: None,
                        mutability: None,
                        elem: Box::new(arg.ty.clone())
                    })
                };
                let altident = syn::Ident::new(&format!("p{}", i), arg.span());
                let altpat = syn::Pat::Ident(syn::PatIdent {
                    by_ref: None,
                    mutability: None,
                    ident: altident,
                    subpat: None
                });
                let altarg = syn::ArgCaptured {
                    pat: altpat,
                    colon_token: syn::Token![:](arg.span()),
                    ty: alt_ty
                };
                altargs.push(altarg);
            },
            syn::FnArg::SelfRef(_) => {
                is_static = false;
            },
            syn::FnArg::SelfValue(_) => {
                is_static = false;
            },
            _ => compile_error(fn_arg.span(),
                "Should be unreachable for normal Rust code")
        }
    }

    let span = Span::call_site();
    let call = match &sig.decl.output {
        syn::ReturnType::Default => {
            syn::Ident::new("call", span)
        },
        syn::ReturnType::Type(_, ty) => {
            match ty.as_ref() {
                syn::Type::Reference(r) => {
                    if let Some(ref lt) = r.lifetime {
                        if lt.ident != "static" {
                            compile_error(r.span(), "Non-'static non-'self lifetimes are not yet supported");
                        }
                    }
                    if r.mutability.is_some() {
                        syn::Ident::new("call_mut", span)
                    } else {
                        syn::Ident::new("call", span)
                    }
                },
                _ => syn::Ident::new("call", span)
            }
        }
    };

    let expectation_ident = syn::Ident::new("Expectation", span);
    let expectations_ident = if is_generic {
        syn::Ident::new("GenericExpectations", span)
    } else {
        syn::Ident::new("Expectations", span)
    };
    let expectations = syn::parse2(
        quote!(#ident::#expectations_ident)
    ).unwrap();
    let expect_obj = if is_generic {
        syn::parse2(quote!(#expectations)).unwrap()
    } else {
        syn::parse2(quote!(#expectations #tg)).unwrap()
    };
    let expect_ts = quote!(#ident::#expectation_ident #tg);
    let expectation: syn::Type = syn::parse2(expect_ts).unwrap();

    MethodTypes{is_static, expectation, expectations,
                call, expect_obj, altargs, matchexprs}
}

/// Convert a special reference type like "&str" into a reference to its owned
/// type like "&String".
fn destrify(ty: &mut syn::Type) {
    if let syn::Type::Reference(ref mut tr) = ty {
        let path_ty: syn::TypePath = syn::parse2(quote!(Path)).unwrap();
        let pathbuf_ty: syn::Type = syn::parse2(quote!(::std::path::PathBuf))
            .unwrap();

        let str_ty: syn::TypePath = syn::parse2(quote!(str)).unwrap();
        let string_ty: syn::Type = syn::parse2(quote!(::std::string::String))
            .unwrap();

        let cstr_ty: syn::TypePath = syn::parse2(quote!(CStr)).unwrap();
        let cstring_ty: syn::Type = syn::parse2(quote!(::std::ffi::CString))
            .unwrap();

        let osstr_ty: syn::TypePath = syn::parse2(quote!(OsStr)).unwrap();
        let osstring_ty: syn::Type = syn::parse2(quote!(::std::ffi::OsString))
            .unwrap();

        match tr.elem.as_ref() {
            syn::Type::Path(ref path) if *path == cstr_ty =>
                *tr.elem = cstring_ty,
            syn::Type::Path(ref path) if *path == osstr_ty =>
                *tr.elem = osstring_ty,
            syn::Type::Path(ref path) if *path == path_ty =>
                *tr.elem = pathbuf_ty,
            syn::Type::Path(ref path) if *path == str_ty =>
                *tr.elem = string_ty,
            _ => (), // Nothing to do
        };
    }
}

/// Manually mock a structure.
///
/// Sometimes `automock` can't be used.  In those cases you can use `mock!`,
/// which basically involves repeating the struct's or trait's definitions.
///
/// The format is:
///
/// * Optional visibility specifier
/// * Real structure name and generics fields
/// * 0 or more methods of the structure, written without bodies, enclosed in
///   a {} block
/// * 0 or more traits to implement for the structure, written like normal
///   traits
///
/// # Examples
///
/// Mock a trait.  This is the simplest use case.
/// ```ignore
/// # use mockall_derive::mock;
/// trait Foo {
///     fn foo(&self, x: u32);
/// }
/// mock!{
///     pub MyStruct<T: Clone + 'static> {
///         fn bar(&self) -> u8;
///     }
///     trait Foo {
///         fn foo(&self, x: u32);
///     }
/// }
/// # fn main() {}
/// ```
///
/// When mocking a generic struct's implementation of a generic trait, use the
/// same namespace for their generic parameters.  For example, if you wanted to
/// mock `Rc`, do
/// ```ignore
/// # use mockall_derive::mock;
/// mock!{
///     pub Rc<T: 'static> {}
///     trait AsRef<T> {
///         fn as_ref(&self) -> &T;
///     }
/// }
/// # fn main() {}
/// ```
/// *not*
/// ```compile_fail
/// # use mockall_derive::mock;
/// mock!{
///     pub Rc<Q: 'static> {}
///     trait AsRef<T: 'static> {
///         fn as_ref(&self) -> &T;
///     }
/// }
/// # fn main() {}
/// ```
/// Associated types can easily be mocked by specifying a concrete type in the
/// `mock!{}` invocation.  But be careful not to reference the associated type
/// in the signatures of any of the trait's methods; repeat the concrete type
/// instead.  For example, do:
/// ```ignore
/// # use mockall_derive::mock;
/// mock!{
///     MyIter {}
///     trait Iterator {
///         type Item=u32;
///
///         fn next(&mut self) -> Option<u32>;
///     }
/// }
/// # fn main() {}
/// ```
/// *not*
/// ```compile_fail
/// # use mockall_derive::mock;
/// mock!{
///     MyIter {}
///     trait Iterator {
///         type Item=u32;
///
///         fn next(&mut self) -> Option<<Self as Iterator>::Item>;
///     }
/// }
/// # fn main() {}
/// ```
#[proc_macro]
pub fn mock(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_mock(item.into()).into()
}

/// Automatically generate mock types for structs and traits.
///
/// This is by far the easiest way to use Mockall.  It works on almost all
/// traits, and almost all structs that have a single `impl` block.  In either
/// case, it will generate a mock struct whose name is the name of the mocked
/// struct/trait prepended with "Mock".  For each method of the original, the
/// mock struct will have a method named `expect_whatever` that allows you to
/// set expectations.  There will also be one `checkpoint` method that calls
/// [`checkpoint`] for every single method.
///
/// # Examples
///
/// The simplest use case is mocking a no-frills trait
/// ```ignore
/// # use mockall_derive::*;
/// #[automock]
/// pub trait Foo {
///     fn foo(&self, key: i16);
/// }
///
/// let mock = MockFoo::new();
/// ```
///
/// Mocking a structure:
/// ```ignore
/// # use mockall_derive::*;
/// struct Foo {}
/// #[automock]
/// impl Foo {
///     fn foo(&self) -> u32 {
///         // ...
///         # unimplemented!()
///     }
/// }
/// ```
///
/// Mocking a trait with associated types requires adding a metaitem to the
/// attribute:
/// ```ignore
/// # use mockall_derive::*;
/// #[automock(type Item=u32;)]
/// trait Foo {
///     type Item;
///     fn foo(&self) -> Self::Item;
/// }
/// ```
///
/// Finally, `#[automock]` can also mock foreign functions.  This requires
/// another metaitem to specify the mock module name.
///
/// ```ignore
/// # use mockall_derive::*;
/// #[automock(mod mock_ffi;)]
/// extern "C" {
///     pub fn foo() -> u32;
/// }
/// ```
///
/// [`checkpoint`]: ../mockall/struct.Expectations.html#method.checkpoint
#[proc_macro_attribute]
pub fn automock(attrs: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let input: proc_macro2::TokenStream = input.into();
    let mut output = input.clone();
    output.extend(do_automock(attrs.into(), input));
    output.into()
}
