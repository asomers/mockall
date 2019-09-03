// vim: tw=80
//! Proc Macros for use with Mockall
//!
//! You probably don't want to use this crate directly.  Instead, you use use
//! its reexports via the [`mockall`](https://docs.rs/mockall/latest/mockall)
//! crate.

#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use std::{
    collections::HashMap,
    convert::identity,
    iter::FromIterator,
    mem
};
use syn::{
    *,
    punctuated::Punctuated,
    spanned::Spanned
};

mod automock;
mod expectation;
mod mock;
use crate::automock::do_automock;
use crate::mock::{Mock, do_mock};
use crate::expectation::Expectation;

struct MethodTypes {
    is_static: bool,
    /// Is the Expectation type generic?  This can be true even if the method is
    /// not generic
    is_expectation_generic: bool,
    /// Type of Expectation returned by the expect method
    expectation: Type,
    /// Generics applicable to the Expectation object
    expectation_generics: Generics,
    /// Input types for the Expectation object
    expectation_inputs: Punctuated<FnArg, Token![,]>,
    /// Type of Expectations container used by the expect method, without its
    /// generics fields
    expectations: Type,
    /// Type of Expectations object stored in the mock structure, with generics
    /// fields
    expect_obj: Type,
    /// Method to call when invoking the expectation
    call: Ident,
    /// Expressions that should be used for Expectation::call's arguments
    call_exprs: Punctuated<TokenStream, Token![,]>,
    /// Method's argument list
    inputs: Punctuated<FnArg, Token![,]>,
    /// Output type of the Expectation, which may be a little bit more general
    /// than the output type of the original method
    output: ReturnType,
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

// If there are any closures in the argument list, turn them into boxed
// functions
fn declosurefy(gen: &Generics, args: &Punctuated<FnArg, Token![,]>) -> 
    (Generics, Punctuated<FnArg, Token![,]>, Punctuated<TokenStream, Token![,]>)
{
    let mut hm = HashMap::new();

    let mut save_fn_types = |ident: &Ident, tpb: &TypeParamBound| {
        if let TypeParamBound::Trait(tb) = tpb {
            let fident = &tb.path.segments.last().unwrap().ident;
            if ["Fn", "FnMut", "FnOnce"].iter().any(|s| fident == *s) {
                let newty: Type = parse2(quote!(Box<dyn #tb>)).unwrap();
                let subst_ty: Type = parse2(quote!(#ident)).unwrap();
                assert!(hm.insert(subst_ty, newty).is_none(),
                    "A generic parameter had two Fn bounds?");
            }
        }
    };

    // First, build a HashMap of all Fn generic types
    for g in gen.params.iter() {
        if let GenericParam::Type(tp) = g {
            for tpb in tp.bounds.iter() {
                save_fn_types(&tp.ident, tpb);
            }
        }
    }
    if let Some(wc) = &gen.where_clause {
        for pred in wc.predicates.iter() {
            if let WherePredicate::Type(pt) = pred {
                let bounded_ty = &pt.bounded_ty;
                if let Ok(ident) = parse2::<Ident>(quote!(#bounded_ty)) {
                    for tpb in pt.bounds.iter() {
                        save_fn_types(&ident, tpb);
                    }
                } else {
                    // We can't yet handle where clauses this complicated
                }
            }
        }
    }

    // Then remove those types from both the Generics' params and where clause
    let should_remove = |ident: &Ident| {
            let ty: Type = parse2(quote!(#ident)).unwrap();
            if hm.contains_key(&ty) {
                true
            } else {
                false
            }
    };
    let params = Punctuated::from_iter(gen.params.iter().filter(|g| {
        if let GenericParam::Type(tp) = g {
            !should_remove(&tp.ident)
        } else {
            true
        }
    }).cloned());
    let mut wc2 = gen.where_clause.clone();
    if let Some(wc) = &mut wc2 {
        wc.predicates = Punctuated::from_iter(wc.predicates.iter().filter(|wp| {
            if let WherePredicate::Type(pt) = wp {
                let bounded_ty = &pt.bounded_ty;
                if let Ok(ident) = parse2::<Ident>(quote!(#bounded_ty)) {
                    !should_remove(&ident)
                } else {
                    // We can't yet handle where clauses this complicated
                    true
                }
            } else {
                true
            }
        }).cloned());
    }
    let outg = Generics {
        lt_token: if params.is_empty() { None } else { gen.lt_token.clone() },
        gt_token: if params.is_empty() { None } else { gen.gt_token.clone() },
        params,
        where_clause: wc2
    };

    // Next substitute Box<Fn> into the arguments
    let outargs = Punctuated::from_iter(args.iter().map(|arg| {
        if let FnArg::Typed(pt) = arg {
            let mut immutable_pt = pt.clone();
            demutify_arg(&mut immutable_pt);
            if let Some(newty) = hm.get(&pt.ty) {
                FnArg::Typed(PatType {
                    attrs: Vec::default(),
                    pat: immutable_pt.pat,
                    colon_token: pt.colon_token.clone(),
                    ty: Box::new(newty.clone())
                })
            } else {
                FnArg::Typed(PatType {
                    attrs: Vec::default(),
                    pat: immutable_pt.pat,
                    colon_token: pt.colon_token.clone(),
                    ty: pt.ty.clone()
                })
            }
        } else {
            arg.clone()
        }
    }));

    // Finally, Box any closure arguments
    // use filter_map to remove the &self argument
    let callargs = Punctuated::from_iter(args.iter().filter_map(|arg| {
        match arg {
            FnArg::Typed(pt) => {
                let mut pt2 = pt.clone();
                demutify_arg(&mut pt2);
                let pat = &pt2.pat;
                if hm.contains_key(&pt.ty) {
                    Some(quote!(Box::new(#pat)))
                } else {
                    Some(quote!(#pat))
                }
            },
            FnArg::Receiver(_) => None,
        }
    }));
    (outg, outargs, callargs)
}

/// Replace any "impl trait" types with "Box<dyn trait>" equivalents
fn deimplify(rt: &mut ReturnType) {
    if let ReturnType::Type(_, ty) = rt {
        if let Type::ImplTrait(ref tit) = &**ty {
            let bounds = &tit.bounds;
            *ty = parse2(quote!(Box<dyn #bounds>)).unwrap();
        }
    }
}

/// Remove any mutability qualifiers from a method's argument list
fn demutify(inputs: &Punctuated<FnArg, token::Comma>)
    -> Punctuated<FnArg, token::Comma>
{
    let mut output = inputs.clone();
    for arg in output.iter_mut() {
        match arg {
            FnArg::Receiver(r) => if r.reference.is_none() {
                r.mutability = None
            },
            FnArg::Typed(pt) => demutify_arg(pt),
        }
    }
    output
}

/// Remove any "mut" from a method argument's binding.
fn demutify_arg(arg: &mut PatType) {
    match *arg.pat {
        Pat::Wild(_) => {
            compile_error(arg.span(),
                "Mocked methods must have named arguments");
        },
        Pat::Ident(ref mut pat_ident) => {
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

/// Replace any references to `Self` in `literal_type` with `actual`.
/// `generics` is the Generics field of the parent struct.  Useful for
/// constructor methods.
fn deselfify(literal_type: &mut Type, actual: &Ident, generics: &Generics) {
    match literal_type {
        Type::Slice(s) => {
            deselfify(s.elem.as_mut(), actual, generics);
        },
        Type::Array(a) => {
            deselfify(a.elem.as_mut(), actual, generics);
        },
        Type::Ptr(p) => {
            deselfify(p.elem.as_mut(), actual, generics);
        },
        Type::Reference(r) => {
            deselfify(r.elem.as_mut(), actual, generics);
        },
        Type::Tuple(tuple) => {
            for elem in tuple.elems.iter_mut() {
                deselfify(elem, actual, generics);
            }
        }
        Type::Path(type_path) => {
            if let Some(ref _qself) = type_path.qself {
                compile_error(type_path.span(), "QSelf is TODO");
            }
            let p = &mut type_path.path;
            for seg in p.segments.iter_mut() {
                if seg.ident == "Self" {
                    seg.ident = actual.clone();
                    if let PathArguments::None = seg.arguments {
                        if !generics.params.is_empty() {
                            let args = Punctuated::from_iter(
                                generics.params.iter().map(|gp| {
                                    let ident = match gp {
                                        GenericParam::Type(tp) => &tp.ident,
                                        GenericParam::Lifetime(ld) =>
                                            &ld.lifetime.ident,
                                        GenericParam::Const(cp) => &cp.ident,
                                    };
                                    parse2::<GenericArgument>(
                                        quote!(#ident)
                                    ).unwrap()
                                })
                            );
                            seg.arguments = PathArguments::AngleBracketed(
                                AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: generics.lt_token.unwrap().clone(),
                                    args,
                                    gt_token: generics.gt_token.unwrap().clone(),

                                }
                            );
                        }
                    } else {
                        compile_error(seg.arguments.span(),
                            "Type arguments after Self are unexpected");
                    }
                }
                if let PathArguments::AngleBracketed(abga) = &mut seg.arguments
                {
                    for arg in abga.args.iter_mut() {
                        match arg {
                            GenericArgument::Type(ty) =>
                                deselfify(ty, actual, generics),
                            GenericArgument::Binding(b) =>
                                deselfify(&mut b.ty, actual, generics),
                            _ => /* Nothing to do */(),
                        }
                    }
                }
            }
        },
        Type::Paren(p) => {
            deselfify(p.elem.as_mut(), actual, generics);
        },
        Type::Group(g) => {
            deselfify(g.elem.as_mut(), actual, generics);
        },
        Type::Macro(_) | Type::Verbatim(_) => {
            compile_error(literal_type.span(),
                "mockall_derive does not support this type as a return argument");
        },
        Type::ImplTrait(_) => {
            panic!("deimplify should've already been run on this output type");
        },
        Type::TraitObject(tto) => {
            // Change types like `dyn Self` into `MockXXX`.  For now,
            // don't worry about multiple trait bounds, because they aren't very
            // useful in combination with Self.
            if tto.bounds.len() == 1 {
                if let TypeParamBound::Trait(t) = tto.bounds.first().unwrap() {
                    // No need to substitute Self for the full path, because
                    // traits can't return "impl Trait", and structs can't
                    // return "impl Self" (because Self, in that case, isn't a
                    // trait).  However, we do need to recurse to deselfify any
                    // generics and associated types.
                    let tp = TypePath{qself: None, path: t.path.clone()};
                    let mut path = Type::Path(tp);
                    deselfify(&mut path, actual, generics);
                    let new_type: Type = parse2(quote!(dyn #path)).unwrap();
                    *literal_type = new_type;
                }
            }
        },
        Type::BareFn(_) => {
            /* Bare functions can't have Self arguments.  Nothing to do */
        },
        Type::Infer(_) | Type::Never(_) =>
        {
            /* Nothing to do */
        },
        _ => compile_error(literal_type.span(), "Unsupported type"),
    }
}

fn supersuperfy_path(path: &mut Path) {
        if let Some(t) = path.segments.first() {
            if t.ident == "super" {
                let mut ident = format_ident!("super");
                ident.set_span(path.segments.span());
                let ps = PathSegment {
                    ident,
                    arguments: PathArguments::None
                };
                path.segments.insert(0, ps.clone());
                path.segments.insert(0, ps);
            }
        }
}

/// Replace any references to `super::X` in `original` with `super::super::X`.
fn supersuperfy(original: &Type) -> Type {
    let mut output = original.clone();
    fn recurse(t: &mut Type) {
        match t {
            Type::Slice(s) => {
                supersuperfy(s.elem.as_mut());
            },
            Type::Array(a) => {
                supersuperfy(a.elem.as_mut());
            },
            Type::Ptr(p) => {
                supersuperfy(p.elem.as_mut());
            },
            Type::Reference(r) => {
                supersuperfy(r.elem.as_mut());
            },
            Type::BareFn(bfn) => {
                if let ReturnType::Type(_, ref mut bt) = bfn.output {
                    let new_bt = Box::new(supersuperfy(bt.as_ref()));
                    mem::replace(bt, new_bt);
                }
                for input in bfn.inputs.iter_mut() {
                    input.ty = supersuperfy(&input.ty);
                }
            },
            Type::Tuple(tuple) => {
                for elem in tuple.elems.iter_mut() {
                    supersuperfy(elem);
                }
            }
            Type::Path(type_path) => {
                if let Some(ref _qself) = type_path.qself {
                    compile_error(type_path.span(), "QSelf is TODO");
                }
                supersuperfy_path(&mut type_path.path)
            },
            Type::Paren(p) => {
                supersuperfy(p.elem.as_mut());
            },
            Type::Group(g) => {
                supersuperfy(g.elem.as_mut());
            },
            Type::Macro(_) | Type::Verbatim(_) => {
                compile_error(t.span(),
                    "mockall_derive does not support this type in this position");
            },
            Type::ImplTrait(_) => {
                panic!("deimplify should've already been run on this output type");
            },
            Type::TraitObject(tto) => {
                for bound in tto.bounds.iter_mut() {
                    if let TypeParamBound::Trait(tb) = bound {
                        supersuperfy_path(&mut tb.path)
                    }
                }
            },
            Type::Infer(_) | Type::Never(_) =>
            {
                /* Nothing to do */
            },
            _ => compile_error(t.span(), "Unsupported type"),
        }
    }
    recurse(&mut output);
    output
}

/// Generate a mock identifier from the regular one: eg "Foo" => "MockFoo"
fn gen_mock_ident(ident: &Ident) -> Ident {
    format_ident!("Mock{}", ident)
}

/// Generate an identifier for the mock struct's private module: eg "Foo" =>
/// "__mock_Foo"
fn gen_mod_ident(struct_: &Ident, trait_: Option<&Ident>) -> Ident {
    if let Some(t) = trait_ {
        format_ident!("__mock_{}_{}", struct_, t)
    } else {
        format_ident!("__mock_{}", struct_)
    }
}

/// Combine two Generics structs, producing a new one that has the union of
/// their parameters.
fn merge_generics(x: &Generics, y: &Generics) -> Generics {
    /// Compare only the identifiers of two GenericParams
    fn cmp_gp_idents(x: &GenericParam, y: &GenericParam) -> bool {
        use GenericParam::*;

        match (x, y) {
            (Type(xtp), Type(ytp)) => xtp.ident == ytp.ident,
            (Lifetime(xld), Lifetime(yld)) => xld.lifetime == yld.lifetime,
            (Const(xc), Const(yc)) => xc.ident == yc.ident,
            _ => false
        }
    }

    /// Compare only the identifiers of two WherePredicates
    fn cmp_wp_idents(x: &WherePredicate, y: &WherePredicate) -> bool {
        use WherePredicate::*;

        match (x, y) {
            (Type(xpt), Type(ypt)) => xpt.bounded_ty == ypt.bounded_ty,
            (Lifetime(xpl), Lifetime(ypl)) => xpl.lifetime == ypl.lifetime,
            (Eq(xeq), Eq(yeq)) => xeq.lhs_ty == yeq.lhs_ty,
            _ => false
        }
    }

    let mut out = if x.lt_token.is_none() {
        y.clone()
    } else if y.lt_token.is_none() {
        x.clone()
    } else {
        let mut out = x.clone();
        // First merge the params
        'outer_param: for yparam in y.params.iter() {
            // XXX: O(n^2) loop
            for outparam in out.params.iter_mut() {
                if cmp_gp_idents(outparam, yparam) {
                    if let (GenericParam::Type(ref mut ot),
                            GenericParam::Type(yt)) = (outparam, yparam)
                    {
                        ot.attrs.extend(yt.attrs.iter().cloned());
                        ot.colon_token = ot.colon_token.or(yt.colon_token);
                        ot.eq_token = ot.eq_token.or(yt.eq_token);
                        if ot.default.is_none() {
                            ot.default = yt.default.clone();
                        }
                        // XXX this might result in duplicate bounds
                        ot.bounds.extend(yt.bounds.iter().cloned());
                    }
                    continue 'outer_param;
                }
            }
            out.params.push(yparam.clone());
        }
        out
    };
    // Then merge the where clauses
    match (&mut out.where_clause, &y.where_clause) {
        (_, None) => (),
        (None, Some(wc)) => out.where_clause = Some(wc.clone()),
        (Some(out_wc), Some(y_wc)) => {
            'outer_wc: for ypred in y_wc.predicates.iter() {
                // XXX: O(n^2) loop
                for outpred in out_wc.predicates.iter_mut() {
                    if cmp_wp_idents(outpred, ypred) {
                        if let (WherePredicate::Type(ref mut ot),
                                WherePredicate::Type(yt)) = (outpred, ypred)
                        {
                            match (&mut ot.lifetimes, &yt.lifetimes) {
                                (_, None) => (),
                                (None, Some(bl)) =>
                                    ot.lifetimes = Some(bl.clone()),
                                (Some(obl), Some(ybl)) =>
                                    // XXX: might result in duplicates
                                    obl.lifetimes.extend(
                                        ybl.lifetimes.iter().cloned()),
                            };
                            // XXX: might result in duplicate bounds
                            ot.bounds.extend(yt.bounds.iter().cloned())
                        }
                        continue 'outer_wc;
                    }
                }
                out_wc.predicates.push(ypred.clone());
            }
        }
    }
    out
}

/// Split a generics list into two: one for types, the other for lifetimes
fn split_lifetimes(generics: Generics) -> (Generics, Generics) {
    if generics.lt_token.is_none() {
        return (generics, Generics::default());
    }

    let (ogv, olv): (Vec<_>, Vec<_>) = generics.params.into_iter()
        .map(|p| {
            if let GenericParam::Lifetime(_) = p {
                (None, Some(p))
            } else {
                (Some(p), None)
            }
        }).unzip();
    let gv = ogv.into_iter().filter_map(identity).collect::<Vec<_>>();
    let lv = olv.into_iter().filter_map(identity).collect::<Vec<_>>();
    let lg = if lv.is_empty() {
        Generics::default()
    } else {
        Generics {
            lt_token: generics.lt_token.clone(),
            gt_token: generics.gt_token.clone(),
            params: Punctuated::from_iter(lv.into_iter()),
            // XXX this assumes that none of the lifetimes are referenced by the
            // where clause
            where_clause: None
        }
    };
    let tg = if gv.is_empty() {
        Generics::default()
    } else {
        Generics {
            lt_token: generics.lt_token,
            gt_token: generics.gt_token,
            params: Punctuated::from_iter(gv.into_iter()),
            // XXX this assumes that none of the lifetimes are referenced by the
            // where clause
            where_clause: generics.where_clause
        }
    };

    (tg, lg)
}

/// Return the visibility that should be used for expectation!, given the
/// original method's visibility.
///
/// # Arguments
/// - `vis`:    Original visibility of the item
/// - `levels`: How many modules will the mock item be nested in?
fn expectation_visibility(vis: &Visibility, levels: u32)
    -> Visibility
{
    debug_assert!(levels > 0);
    let in_token = Token![in](vis.span());
    let super_token = Token![super](vis.span());
    match vis {
        Visibility::Inherited => {
            // Private items need pub(in super::[...]) for each level
            let mut path = Path::from(super_token);
            for _ in 1..levels {
                path.segments.push(super_token.into());
            }
            Visibility::Restricted(VisRestricted{
                pub_token: Token![pub](vis.span()),
                paren_token: token::Paren::default(),
                in_token: Some(in_token),
                path: Box::new(path)
            })
        },
        Visibility::Restricted(vr) => {
            // crate => don't change
            // in crate::* => don't change
            // super => in super::super::super
            // self => in super::super
            // in anything_else => super::super::anything_else
            if vr.path.segments.first().unwrap().ident == "crate" {
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
fn method_types(sig: &Signature, generics: Option<&Generics>) -> MethodTypes {
    let mut is_static = true;
    let ident = &sig.ident;
    let (expectation_generics, expectation_inputs, call_exprs) =
        declosurefy(&sig.generics, &sig.inputs);
    let (merged_g, _) = split_lifetimes(if let Some(g) = generics {
        merge_generics(&g, &expectation_generics)
    } else {
        sig.generics.clone()
    });
    let inputs = demutify(&sig.inputs);
    let (_ig, tg, _wc) = merged_g.split_for_impl();
    for fn_arg in expectation_inputs.iter() {
        match fn_arg {
            FnArg::Typed(_) => {},
            FnArg::Receiver(_) => {
                is_static = false;
            },
        }
    }

    let call = match &sig.output {
        ReturnType::Default => {
            format_ident!("call")
        },
        ReturnType::Type(_, ty) => {
            match ty.as_ref() {
                Type::Reference(r) => {
                    if let Some(ref lt) = r.lifetime {
                        if lt.ident != "static" {
                            compile_error(r.span(), "Non-'static non-'self lifetimes are not yet supported");
                        }
                    }
                    if r.mutability.is_some() {
                        format_ident!("call_mut")
                    } else {
                        format_ident!("call")
                    }
                },
                _ => format_ident!("call")
            }
        }
    };

    let is_expectation_generic = expectation_generics.params.iter().any(|p| {
            if let GenericParam::Type(_) = p {
                true
            } else {
                false
            }
        }) || expectation_generics.where_clause.is_some() ||
        (is_static && generics.filter(|g| !g.params.is_empty()).is_some());

    let expectation_ident = format_ident!("Expectation");
    let expectations_ident = if is_expectation_generic {
        format_ident!("GenericExpectations")
    } else {
        format_ident!("Expectations")
    };
    let expectations = parse2(
        quote!(#ident::#expectations_ident)
    ).unwrap();
    let expect_obj = if is_expectation_generic {
        parse2(quote!(#expectations)).unwrap()
    } else {
        parse2(quote!(#expectations #tg)).unwrap()
    };
    let expect_ts = quote!(#ident::#expectation_ident #tg);
    let expectation: Type = parse2(expect_ts).unwrap();
    let mut output = sig.output.clone();
    deimplify(&mut output);

    MethodTypes{is_static, is_expectation_generic, expectation,
                expectation_generics, expectation_inputs, expectations, call,
                expect_obj, call_exprs, inputs, output}
}

/// Manually mock a structure.
///
/// See detailed docs in [`mock`](https://docs.rs/mockall/latest/mockall/macro.mock.html).
#[proc_macro]
pub fn mock(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_mock(item.into()).into()
}

/// Automatically generate mock types for structs and traits.
///
/// See detailed docs in [`automock`](https://docs.rs/mockall/latest/mockall/macro.automock.html).
#[proc_macro_attribute]
pub fn automock(attrs: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let input: proc_macro2::TokenStream = input.into();
    let mut output = input.clone();
    output.extend(do_automock(attrs.into(), input));
    output.into()
}

#[cfg(test)]
mod t {
    use super::*;

    #[test]
    fn merge_generics() {
        let mut g1: Generics = parse2(quote!(<T: 'static, V: Copy> )).unwrap();
        let wc1: WhereClause = parse2(quote!(where T: Default)).unwrap();
        g1.where_clause = Some(wc1);

        let mut g2: Generics = parse2(quote!(<Q: Send, V: Clone>)).unwrap();
        let wc2: WhereClause = parse2(quote!(where T: Sync, Q: Debug)).unwrap();
        g2.where_clause = Some(wc2);

        let gm = super::merge_generics(&g1, &g2);
        let gm_wc = &gm.where_clause;

        let ge: Generics = parse2(quote!(
                <T: 'static, V: Copy + Clone, Q: Send>
        )).unwrap();
        let wce: WhereClause = parse2(quote!(
            where T: Default + Sync, Q: Debug
        )).unwrap();

        assert_eq!(quote!(#ge #wce).to_string(),
                   quote!(#gm #gm_wc).to_string());
    }
}
