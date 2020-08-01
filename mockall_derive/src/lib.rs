// vim: tw=80
//! Proc Macros for use with Mockall
//!
//! You probably don't want to use this crate directly.  Instead, you use use
//! its reexports via the [`mockall`](https://docs.rs/mockall/latest/mockall)
//! crate.

#![cfg_attr(feature = "nightly_derive", feature(proc_macro_diagnostic))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote};
use std::{
    collections::{HashMap, HashSet},
    env,
    iter::FromIterator,
};
use syn::{
    *,
    punctuated::Punctuated,
    spanned::Spanned
};

mod automock;
//mod expectation;
mod mock_function;
mod mock_item;
mod mock_item_struct;
mod mock_trait;
mod mockable_item;
mod mockable_struct;
use crate::automock::Attrs;
use crate::mockable_struct::MockableStruct;
use crate::mock_item::MockItem;
use crate::mock_item_struct::MockItemStruct;
use crate::mockable_item::MockableItem;

cfg_if! {
    // proc-macro2's Span::unstable method requires the nightly feature, and it
    // doesn't work in test mode.
    // https://github.com/alexcrichton/proc-macro2/issues/159
    if #[cfg(all(feature = "nightly_derive", not(test)))] {
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
            hm.contains_key(&ty)
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
        lt_token: if params.is_empty() { None } else { gen.lt_token },
        gt_token: if params.is_empty() { None } else { gen.gt_token },
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
                    colon_token: pt.colon_token,
                    ty: Box::new(newty.clone())
                })
            } else {
                FnArg::Typed(PatType {
                    attrs: Vec::default(),
                    pat: immutable_pt.pat,
                    colon_token: pt.colon_token,
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
fn demutify(inputs: &mut Punctuated<FnArg, token::Comma>) {
    for arg in inputs.iter_mut() {
        match arg {
            FnArg::Receiver(r) => if r.reference.is_none() {
                r.mutability = None
            },
            FnArg::Typed(pt) => demutify_arg(pt),
        }
    }
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
                                    match gp {
                                        GenericParam::Type(tp) => {
                                            let ident = tp.ident.clone();
                                            GenericArgument::Type(
                                                Type::Path(
                                                    TypePath {
                                                        qself: None,
                                                        path: Path::from(ident)
                                                    }
                                                )
                                            )
                                        },
                                        GenericParam::Lifetime(ld) =>{
                                            GenericArgument::Lifetime(
                                                ld.lifetime.clone()
                                            )
                                        }
                                        _ => unimplemented!(),
                                    }
                                })
                            );
                            seg.arguments = PathArguments::AngleBracketed(
                                AngleBracketedGenericArguments {
                                    colon2_token: None,
                                    lt_token: generics.lt_token.unwrap(),
                                    args,
                                    gt_token: generics.gt_token.unwrap(),

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

fn find_lifetimes_in_tpb(bound: &TypeParamBound) -> HashSet<Lifetime> {
    let mut ret = HashSet::default();
    match bound {
        TypeParamBound::Lifetime(lt) => {
            ret.insert(lt.clone());
        },
        TypeParamBound::Trait(tb) => {
            ret.extend(find_lifetimes_in_path(&tb.path));
        },
    };
    ret
}

fn find_lifetimes_in_path(path: &Path) -> HashSet<Lifetime> {
    let mut ret = HashSet::default();
    for seg in path.segments.iter() {
        if let PathArguments::AngleBracketed(abga) = &seg.arguments {
            for arg in abga.args.iter() {
                match arg {
                    GenericArgument::Lifetime(lt) => {
                        ret.insert(lt.clone());
                    },
                    GenericArgument::Type(ty) => {
                        ret.extend(find_lifetimes(&ty));
                    },
                    GenericArgument::Binding(b) => {
                        ret.extend(find_lifetimes(&b.ty));
                    },
                    GenericArgument::Constraint(c) => {
                        for bound in c.bounds.iter() {
                            ret.extend(find_lifetimes_in_tpb(bound));
                        }
                    },
                    GenericArgument::Const(_) => ()
                }
            }
        }
    }
    ret
}

fn find_lifetimes(ty: &Type) -> HashSet<Lifetime> {
    match ty {
        Type::Array(ta) => find_lifetimes(ta.elem.as_ref()),
        Type::Group(tg) => find_lifetimes(tg.elem.as_ref()),
        Type::Infer(_ti) => HashSet::default(),
        Type::Never(_tn) => HashSet::default(),
        Type::Paren(tp) => find_lifetimes(tp.elem.as_ref()),
        Type::Path(tp) => {
            let mut ret = find_lifetimes_in_path(&tp.path);
            if let Some(qs) = &tp.qself {
                ret.extend(find_lifetimes(qs.ty.as_ref()));
            }
            ret
        },
        Type::Ptr(tp) => find_lifetimes(tp.elem.as_ref()),
        Type::Reference(tr) => {
            let mut ret = find_lifetimes(tr.elem.as_ref());
            if let Some(lt) = &tr.lifetime {
                ret.insert(lt.clone());
            }
            ret
        },
        Type::Slice(ts) => find_lifetimes(ts.elem.as_ref()),
        Type::TraitObject(tto) => {
            let mut ret = HashSet::default();
            for bound in tto.bounds.iter() {
                ret.extend(find_lifetimes_in_tpb(bound));
            }
            ret
        }
        Type::Tuple(tt) => {
            let mut ret = HashSet::default();
            for ty in tt.elems.iter() {
                ret.extend(find_lifetimes(ty));
            }
            ret
        },
        Type::ImplTrait(tit) => {
            let mut ret = HashSet::default();
            for tpb in tit.bounds.iter() {
                ret.extend(find_lifetimes_in_tpb(tpb));
            }
            ret
        },
        _ => {
            compile_error(ty.span(), "unsupported type in this context");
            HashSet::default()
        }

    }
}

fn format_attrs(attrs: &[syn::Attribute], include_docs: bool) -> TokenStream {
    let mut out = TokenStream::new();
    for attr in attrs {
        let is_doc = attr.path.get_ident().map(|i| i == "doc").unwrap_or(false);
        if !is_doc || include_docs {
            attr.to_tokens(&mut out);
        }
    }
    out
}

fn supersuperfy_path(path: &mut Path, levels: i32) {
        if let Some(t) = path.segments.first() {
            if t.ident == "super" {
                let mut ident = format_ident!("super");
                ident.set_span(path.segments.span());
                let ps = PathSegment {
                    ident,
                    arguments: PathArguments::None
                };
                for _ in 0..levels {
                    path.segments.insert(0, ps.clone());
                }
            }
        }
}

/// Replace any references to `super::X` in `original` with `super::super::X`.
fn supersuperfy(original: &Type, levels: i32) -> Type {
    let mut output = original.clone();
    fn recurse(t: &mut Type, levels: i32) {
        match t {
            Type::Slice(s) => {
                recurse(s.elem.as_mut(), levels);
            },
            Type::Array(a) => {
                recurse(a.elem.as_mut(), levels);
            },
            Type::Ptr(p) => {
                recurse(p.elem.as_mut(), levels);
            },
            Type::Reference(r) => {
                recurse(r.elem.as_mut(), levels);
            },
            Type::BareFn(bfn) => {
                if let ReturnType::Type(_, ref mut bt) = bfn.output {
                    recurse(bt.as_mut(), levels);
                }
                for input in bfn.inputs.iter_mut() {
                    recurse(&mut input.ty, levels);
                }
            },
            Type::Tuple(tuple) => {
                for elem in tuple.elems.iter_mut() {
                    recurse(elem, levels);
                }
            }
            Type::Path(type_path) => {
                if let Some(ref _qself) = type_path.qself {
                    compile_error(type_path.span(), "QSelf is TODO");
                }
                supersuperfy_path(&mut type_path.path, levels);
            },
            Type::Paren(p) => {
                recurse(p.elem.as_mut(), levels);
            },
            Type::Group(g) => {
                recurse(g.elem.as_mut(), levels);
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
                        supersuperfy_path(&mut tb.path, levels)
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
    recurse(&mut output, levels);
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

    let mut out = if x.lt_token.is_none() && x.where_clause.is_none() {
        y.clone()
    } else if y.lt_token.is_none() && y.where_clause.is_none() {
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
                        if ot.bounds != yt.bounds {
                            ot.bounds.extend(yt.bounds.iter().cloned());
                        }
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
                            if ot.bounds != yt.bounds {
                                ot.bounds.extend(yt.bounds.iter().cloned())
                            }
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

/// Transform a Vec of lifetimes into a Generics
fn lifetimes_to_generics(lv: Vec<GenericParam>) -> Generics {
    if lv.is_empty() {
            Generics::default()
    } else {
        Generics {
            lt_token: Some(Token![<](lv[0].span())),
            gt_token: Some(Token![>](lv[0].span())),
            params: Punctuated::from_iter(lv.into_iter()),
            // XXX this assumes that none of the lifetimes are referenced by the
            // where clause
            where_clause: None
        }
    }
}

/// Split a generics list into three: one for type generics, one for lifetime
/// generics that relate to the arguments only, and one for lifetime generics
/// that relate to the return type.
fn split_lifetimes(
    generics: Generics,
    args: &Punctuated<FnArg, Token![,]>,
    rt: &ReturnType)
    -> (Generics, Generics, Generics)
{
    if generics.lt_token.is_none() {
        return (generics, Generics::default(), Generics::default());
    }

    // Check which lifetimes are referenced by the arguments
    let mut alts = HashSet::<Lifetime>::default();
    for arg in args {
        match arg {
            FnArg::Receiver(r) => {
                if let Some((_, olt)) = &r.reference {
                    if let Some(lt) = olt {
                        alts.insert(lt.clone());
                    }
                }
            },
            FnArg::Typed(pt) => {
                alts.extend(find_lifetimes(pt.ty.as_ref()))
            },
        };
    };

    // Check which lifetimes are referenced by the return type
    let rlts = match rt {
        ReturnType::Default => HashSet::default(),
        ReturnType::Type(_, ty) => find_lifetimes(ty)
    };

    let mut tv = Vec::new();
    let mut alv = Vec::new();
    let mut rlv = Vec::new();
    for p in generics.params {
        match &p {
            GenericParam::Lifetime(ltd) if rlts.contains(&ltd.lifetime) =>
                rlv.push(p),
            GenericParam::Lifetime(ltd) if alts.contains(&ltd.lifetime) =>
                alv.push(p),
            GenericParam::Lifetime(_) => {
                // Probably a lifetime parameter from the impl block that isn't
                // used by this particular method
            },
            _ => tv.push(p)
        }
    }

    let alg = lifetimes_to_generics(alv);
    let rlg = lifetimes_to_generics(rlv);

    let tg = if tv.is_empty() {
        Generics::default()
    } else {
        Generics {
            lt_token: generics.lt_token,
            gt_token: generics.gt_token,
            params: Punctuated::from_iter(tv.into_iter()),
            // XXX this assumes that none of the lifetimes are referenced by the
            // where clause
            where_clause: generics.where_clause
        }
    };

    (tg, alg, rlg)
}

/// Return the visibility that should be used for expectation!, given the
/// original method's visibility.
///
/// # Arguments
/// - `vis`:    Original visibility of the item
/// - `levels`: How many modules will the mock item be nested in?
fn expectation_visibility(vis: &Visibility, levels: i32)
    -> Visibility
{
    debug_assert!(levels >= 0);
    if levels == 0 {
        return vis.clone();
    }

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

fn staticize(generics: &Generics) -> Generics {
    let mut ret = generics.clone();
    for lt in ret.lifetimes_mut() {
        lt.lifetime = Lifetime::new("'static", Span::call_site());
    };
    ret
}

fn mock_it<M: Into<MockableItem>>(inputs: M) -> TokenStream
{
    let mockable: MockableItem = inputs.into();
    let mock = MockItem::from(mockable);
    let ts = mock.into_token_stream();
    if env::var("MOCKALL_DEBUG").is_ok() {
        println!("{}", ts);
    }
    ts
}

#[proc_macro]
pub fn mock(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let item: MockableStruct = match syn::parse2(input.into()) {
        Ok(mock) => mock,
        Err(err) => {
            return err.to_compile_error().into();
        }
    };
    mock_it(item).into()
}

#[proc_macro_attribute]
pub fn automock(attrs: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let attrs: proc_macro2::TokenStream = attrs.into();
    let input: proc_macro2::TokenStream = input.into();
    do_automock(attrs, input).into()
}

fn do_automock(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let mut output = input.clone();
    let attrs: Attrs = match parse2(attrs) {
        Ok(a) => a,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    let item: Item = match parse2(input) {
        Ok(item) => item,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    output.extend(mock_it((attrs, item)));
    output
}

#[cfg(test)]
mod t {
    use super::*;

/// Various tests for overall code generation that are hard or impossible to
/// write as integration tests
mod automock {
    use std::str::FromStr;
    use super::super::*;

    #[test]
    fn doc_comments() {
        let code = r#"
            mod foo {
                /// Function docs
                pub fn bar() { unimplemented!() }
            }
        "#;
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let attrs_ts = proc_macro2::TokenStream::from_str("").unwrap();
        let output = do_automock(attrs_ts, ts)
            .to_string()
            // Strip spaces so we don't get test regressions due to minor
            // formatting changes
            .replace(" ", "");
        assert!(output.contains(r#"#[doc="Functiondocs"]pubfnbar"#));
    }

    #[test]
    fn method_visibility() {
        let code = r#"
        impl Foo {
            fn foo(&self) {}
            pub fn bar(&self) {}
            pub(super) fn baz(&self) {}
            pub(crate) fn bang(&self) {}
            pub(in super::x) fn bean(&self) {}
        }"#;
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let attrs_ts = proc_macro2::TokenStream::from_str("").unwrap();
        let output = do_automock(attrs_ts, ts).to_string();
        assert!(!output.contains("pub fn foo"));
        assert!(!output.contains("pub fn expect_foo"));
        assert!(output.contains("pub fn bar"));
        assert!(output.contains("pub fn expect_bar"));
        assert!(output.contains("pub ( super ) fn baz"));
        assert!(output.contains("pub ( super ) fn expect_baz"));
        assert!(output.contains("pub ( crate ) fn bang"));
        assert!(output.contains("pub ( crate ) fn expect_bang"));
        assert!(output.contains("pub ( in super :: x ) fn bean"));
        assert!(output.contains("pub ( in super :: x ) fn expect_bean"));
    }

    #[test]
    #[should_panic(expected = "can only mock inline modules")]
    fn external_module() {
        let code = r#"mod foo;"#;
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let attrs_ts = proc_macro2::TokenStream::from_str("").unwrap();
        do_automock(attrs_ts, ts).to_string();
    }

    #[test]
    fn trait_visibility() {
        let code = r#"
        pub(super) trait Foo {}
        "#;
        let attrs_ts = proc_macro2::TokenStream::from_str("").unwrap();
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let output = do_automock(attrs_ts, ts).to_string();
        assert!(output.contains("pub ( super ) struct MockFoo"));
    }
}

mod merge_generics {
    use super::*;

    #[test]
    fn both() {
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

    #[test]
    fn eq() {
        let mut g1: Generics = parse2(quote!(<T: 'static, V: Copy> )).unwrap();
        let wc1: WhereClause = parse2(quote!(where T: Default)).unwrap();
        g1.where_clause = Some(wc1.clone());

        let g2 = g1.clone();

        let gm = super::merge_generics(&g1, &g2);
        let gm_wc = &gm.where_clause;

        assert_eq!(quote!(#g1 #wc1).to_string(),
                   quote!(#gm #gm_wc).to_string());
    }

    #[test]
    fn lhs_only() {
        let mut g1: Generics = parse2(quote!(<T: 'static, V: Copy> )).unwrap();
        let wc1: WhereClause = parse2(quote!(where T: Default)).unwrap();
        g1.where_clause = Some(wc1.clone());

        let g2 = Generics::default();

        let gm = super::merge_generics(&g1, &g2);
        let gm_wc = &gm.where_clause;

        assert_eq!(quote!(#g1 #wc1).to_string(),
                   quote!(#gm #gm_wc).to_string());
    }

    #[test]
    fn lhs_wc_only() {
        let mut g1 = Generics::default();
        let wc1: WhereClause = parse2(quote!(where T: Default)).unwrap();
        g1.where_clause = Some(wc1.clone());

        let g2 = Generics::default();

        let gm = super::merge_generics(&g1, &g2);
        let gm_wc = &gm.where_clause;

        assert_eq!(quote!(#g1 #wc1).to_string(),
                   quote!(#gm #gm_wc).to_string());
    }

    #[test]
    fn rhs_only() {
        let g1 = Generics::default();
        let mut g2: Generics = parse2(quote!(<Q: Send, V: Clone>)).unwrap();
        let wc2: WhereClause = parse2(quote!(where T: Sync, Q: Debug)).unwrap();
        g2.where_clause = Some(wc2.clone());

        let gm = super::merge_generics(&g1, &g2);
        let gm_wc = &gm.where_clause;

        assert_eq!(quote!(#g2 #wc2).to_string(),
                   quote!(#gm #gm_wc).to_string());
    }
}

mod supersuperfy {
    use super::*;

    fn check_supersuperfy(orig: TokenStream, expected: TokenStream) {
        let orig_ty: Type = parse2(orig).unwrap();
        let expected_ty: Type = parse2(expected).unwrap();
        let output = supersuperfy(&orig_ty, 1);
        assert_eq!(format!("{}", quote!(#output)),
                   format!("{}", quote!(#expected_ty)));
    }

    #[test]
    fn array() {
        check_supersuperfy(
            quote!([super::X; n]),
            quote!([super::super::X; n])
        );
    }

    #[test]
    fn barefn() {
        check_supersuperfy(
            quote!(fn(super::A) -> super::B),
            quote!(fn(super::super::A) -> super::super::B)
        );
    }

    #[test]
    fn group() {
        let orig = TypeGroup {
            group_token: token::Group::default(),
            elem: Box::new(parse2(quote!(super::T)).unwrap())
        };
        let expected = TypeGroup {
            group_token: token::Group::default(),
            elem: Box::new(parse2(quote!(super::super::T)).unwrap())
        };
        let output = supersuperfy(&Type::Group(orig), 1);
        assert_eq!(format!("{}", quote!(#output)),
                   format!("{}", quote!(#expected)));
    }

    // Just check that it doesn't panic
    #[test]
    fn infer() {
        check_supersuperfy( quote!(_), quote!(_));
    }

    // Just check that it doesn't panic
    #[test]
    fn never() {
        check_supersuperfy( quote!(!), quote!(!));
    }

    #[test]
    fn paren() {
        check_supersuperfy(
            quote!((super::X)),
            quote!((super::super::X))
        );
    }

    #[test]
    fn path() {
        check_supersuperfy(
            quote!(::super::SuperT<u32>),
            quote!(::super::super::SuperT<u32>)
        );
    }

    #[test]
    fn ptr() {
        check_supersuperfy(
            quote!(*const super::X),
            quote!(*const super::super::X)
        );
    }

    #[test]
    fn reference() {
        check_supersuperfy(
            quote!(&'a mut super::X),
            quote!(&'a mut super::super::X)
        );
    }

    #[test]
    fn slice() {
        check_supersuperfy(
            quote!([super::X]),
            quote!([super::super::X])
        );
    }

    #[test]
    fn trait_object() {
        check_supersuperfy(
            quote!(dyn super::X + super::Y),
            quote!(dyn super::super::X + super::super::Y)
        );
    }

    #[test]
    fn tuple() {
        check_supersuperfy(
            quote!((super::A, super::B)),
            quote!((super::super::A, super::super::B))
        );
    }
}

}
