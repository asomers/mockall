// vim: tw=80
//! A replacement for the old (omnimacro era) expectation!

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use std::iter::{FromIterator, IntoIterator};
use syn::Token;

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

fn split_args(args: &syn::punctuated::Punctuated<syn::FnArg, Token![,]>)
    -> (syn::punctuated::Punctuated<syn::Pat, Token![,]>,
        syn::punctuated::Punctuated<syn::Type, Token![,]>)
{
    let mut names = syn::punctuated::Punctuated::new();
    let mut types = syn::punctuated::Punctuated::new();
    for fa in args {
        if let syn::FnArg::Captured(ac) = fa {
            names.push(ac.pat.clone());
            types.push(ac.ty.clone());
        }
    }
    (names, types)
}

fn strip_self(args: &syn::punctuated::Punctuated<syn::FnArg, Token![,]>)
    -> syn::punctuated::Punctuated<syn::FnArg, Token![,]>
{
    let mut out = syn::punctuated::Punctuated::new();
    for fa in args {
        if let syn::FnArg::Captured(_) = fa {
            out.push(fa.clone());
        }
    }
    out
}

/// Generate the code that implements an expectation for a single method
pub(crate) fn expectation(attrs: &TokenStream, vis: &syn::Visibility,
    self_ident: Option<&syn::Ident>,
    ident: &syn::Ident,
    generics: &syn::Generics,
    args: &syn::punctuated::Punctuated<syn::FnArg, Token![,]>,
    return_type: &syn::ReturnType,
    altargs: &syn::punctuated::Punctuated<syn::FnArg, Token![,]>,
    matchexprs: &syn::punctuated::Punctuated<syn::Expr, Token![,]>
    ) -> TokenStream
{
    let mut ref_expectation = false;
    let mut ref_mut_expectation = false;
    let mut static_method = true;

    let output = match return_type{
        syn::ReturnType::Default => quote!(()),
        syn::ReturnType::Type(_, ty) => {
            let mut rt = ty.clone();
            crate::deimplify(&mut rt);
            destrify(&mut rt);
            if let Some(i) = self_ident {
                crate::deselfify(&mut rt, i);
            }
            if let syn::Type::Reference(ref tr) = rt.as_ref() {
                if tr.lifetime.as_ref().map_or(false, |lt| lt.ident == "static")
                {
                    // Just a static expectation
                } else {
                    if !tr.mutability.is_some() {
                        ref_expectation = true;
                    } else {
                        ref_mut_expectation = true;
                    }
                    rt = tr.elem.clone();
                }
            }
            quote!(#rt)
        }
    };

    let mut macro_g = TokenStream::new();
    let mut bounded_macro_g = TokenStream::new();
    // Convert the TypeGenerics into a sequence of idents, because that's
    // what static_expectations! expects
    // TODO: once static_expectations! is gone, something like this:
    //generics.split_for_impl().1.to_tokens(&mut macro_g)
    for p in generics.params.iter() {
        if let syn::GenericParam::Type(tp) = p {
            tp.ident.to_tokens(&mut macro_g);
            quote!(#ident: 'static).to_tokens(&mut bounded_macro_g);
        }
    }
    for fa in args {
        static_method &= match fa {
            syn::FnArg::SelfRef(_) | syn::FnArg::SelfValue(_) => false,
            _ => true
        };
    }

    let selfless_args = strip_self(args);
    let (argnames, argty) = split_args(args);
    let (altargnames, altargty) = split_args(altargs);
    if ref_expectation {
        quote!(
            #attrs
            pub mod #ident {
            use ::downcast::*;
            use ::fragile::Fragile;
            use ::predicates_tree::CaseTreeExt;
            use ::std::{
                collections::hash_map::HashMap,
                marker::PhantomData,
                mem,
                ops::{DerefMut, Range},
                sync::Mutex
            };
            use super::*;

            ::mockall::common_matcher!{
                [#macro_g] [#argnames] [#altargnames] [#altargty]
            }

            ::mockall::common_methods!{
                [#macro_g] [#argnames] [#altargnames] [#altargty]
            }

            /// Expectation type for methods taking a `&self` argument and
            /// returning immutable references.  This is the type returned by
            /// the `expect_*` methods.
            #vis struct Expectation<#bounded_macro_g> {
                common: Common<#macro_g>,
                result: Option<#output>,
            }

            impl<#bounded_macro_g> Expectation<#macro_g> {
                #vis fn call(&self, #selfless_args ) -> &#output {
                    self.common.call(#matchexprs);
                    &self.result.as_ref()
                        .expect("Must set return value with return_const")
                }

                /// Return a reference to a constant value from the `Expectation`
                #vis fn return_const(&mut self, o: #output) -> &mut Self {
                    self.result = Some(o);
                    self
                }

                ::mockall::expectation_methods!{
                    #vis [#argnames] [#altargnames] [#altargty]
                }
            }

            impl<#bounded_macro_g> Default for Expectation<#macro_g>
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        result: None
                    }
                }
            }

            ::mockall::expectations_methods!{#vis [#macro_g]}
            impl<#bounded_macro_g> Expectations<#macro_g> {
                /// Simulating calling the real method.  Every current
                /// expectation will be checked in FIFO order and the first one
                /// with matching arguments will be used.
                #vis fn call(&self, #selfless_args ) -> &#output {
                    let n = self.0.len();
                    match self.0.iter()
                        .find(|e| e.matches(#matchexprs) &&
                              (!e.is_done() || n == 1))
                    {
                        None => panic!("No matching expectation found"),
                        Some(e) => e.call(#selfless_args)
                    }
                }
            }
            // The Senc + Sync are required for downcast, since Expectation
            // stores an Option<#output>
            impl<#bounded_macro_g>
                ::mockall::AnyExpectations for Expectations<#macro_g>
                    where #output: Send + Sync
            {}

            ::mockall::generic_expectation_methods!{#vis [#macro_g] [#argty] #output}
            impl GenericExpectations {
                /// Simulating calling the real method.
                #vis fn call<#bounded_macro_g>
                    (&self, #selfless_args ) -> &#output
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let e: &Expectations<#macro_g> = self.store.get(&key)
                        .expect("No matching expectation found")
                        .downcast_ref()
                        .unwrap();
                    e.call(#selfless_args)
                }

                /// Create a new Expectation.
                #vis fn expect<'e, #bounded_macro_g>(&'e mut self)
                    -> &'e mut Expectation<#macro_g>
                    where #output: Send + Sync
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.store.entry(key)
                        .or_insert_with(||
                            Box::new(Expectations::<#macro_g>::new())
                        ).downcast_mut()
                        .unwrap();
                    ee.expect()
                }
            }
        }
        )
    } else if ref_mut_expectation {
        quote!(
            #attrs
            pub mod #ident {
            use ::downcast::*;
            use ::predicates_tree::CaseTreeExt;
            use ::fragile::Fragile;
            use ::std::{
                collections::hash_map::HashMap,
                marker::PhantomData,
                mem,
                ops::{DerefMut, Range},
                sync::{Mutex, MutexGuard}
            };
            use super::*;

            ::mockall::common_matcher!{
                [#macro_g] [#argnames] [#altargnames] [#altargty]
            }

            ::mockall::common_methods!{
                [#macro_g] [#argnames] [#altargnames] [#altargty]
            }

            /// Expectation type for methods taking a `&mut self` argument and
            /// returning references.  This is the type returned by the
            /// `expect_*` methods.
            #vis struct Expectation<#bounded_macro_g> {
                common: Common<#macro_g>,
                result: Option<#output>,
                rfunc: Option<Box<dyn FnMut(#argty) -> #output + Send + Sync>>,
            }

            impl<#bounded_macro_g> Expectation<#macro_g> {
                /// Simulating calling the real method for this expectation
                #vis fn call_mut(&mut self, #selfless_args) -> &mut #output {
                    self.common.call(#selfless_args);
                    if let Some(ref mut f) = self.rfunc {
                        self.result = Some(f(#argnames));
                    }
                    self.result.as_mut()
                        .expect("Must first set return function with returning or return_var")
                }

                /// Convenience method that can be used to supply a return value
                /// for a `Expectation`.  The value will be returned by mutable
                /// reference.
                #vis fn return_var(&mut self, o: #output) -> &mut Self
                {
                    self.result = Some(o);
                    self
                }

                /// Supply a closure that the `Expectation` will use to create its
                /// return value.  The return value will be returned by mutable
                /// reference.
                #vis fn returning<F>(&mut self, f: F) -> &mut Self
                    where F: FnMut(#argty) -> #output + Send + Sync + 'static
                {
                    mem::replace(&mut self.rfunc, Some(Box::new(f)));
                    self
                }

                /// Single-threaded version of [`returning`](#method.returning).
                /// Can be used when the argument or return type isn't `Send`.
                #vis fn returning_st<F>(&mut self, f: F) -> &mut Self
                    where F: FnMut(#argty) -> #output + 'static
                {
                    let mut fragile = Fragile::new(f);
                    let fmut = move |#selfless_args| {
                        (fragile.get_mut())(#argnames)
                    };
                    mem::replace(&mut self.rfunc, Some(Box::new(fmut)));
                    self
                }

                ::mockall::expectation_methods!{
                    #vis [#argnames] [#altargnames] [#altargty]
                }
            }
            impl<#bounded_macro_g> Default for Expectation<#macro_g>
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        result: None,
                        rfunc: None
                    }
                }
            }
            ::mockall::expectations_methods!{#vis [#macro_g]}
            impl<#bounded_macro_g> Expectations<#macro_g> {
                /// Simulating calling the real method.  Every current
                /// expectation will be checked in FIFO order and the first one
                /// with matching arguments will be used.
                #vis fn call_mut(&mut self, #selfless_args ) -> &mut #output {
                    let n = self.0.len();
                    match self.0.iter_mut()
                        .find(|e| e.matches(#matchexprs) &&
                              (!e.is_done() || n == 1))
                    {
                        None => panic!("No matching expectation found"),
                        Some(e) => e.call_mut(#argnames)
                    }
                }
            }
            // The Senc + Sync are required for downcast, since Expectation
            // stores an Option<#output>
            impl<#bounded_macro_g>
                ::mockall::AnyExpectations for Expectations<#macro_g>
                where #output: Send + Sync
            {}

            ::mockall::generic_expectation_methods!{
                #vis [#macro_g] [#argty] #output
            }
            impl GenericExpectations {
                /// Simulating calling the real method.
                #vis fn call_mut<#bounded_macro_g>
                    (&mut self, #selfless_args ) -> &mut #output
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let e: &mut Expectations<#macro_g> = self.store
                        .get_mut(&key)
                        .expect("No matching expectation found")
                        .downcast_mut()
                        .unwrap();
                    e.call_mut(#argnames)
                }

                /// Create a new Expectation.
                #vis fn expect<'e, #bounded_macro_g> (&'e mut self)
                    -> &'e mut Expectation<#macro_g>
                    where #output: Send + Sync
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.store.entry(key)
                        .or_insert_with(||
                            Box::new(Expectations::<#macro_g>::new())
                        ).downcast_mut()
                        .unwrap();
                    ee.expect()
                }
            }
        }
        )
    } else if static_method {
        let mut withfty: syn::punctuated::Punctuated<syn::Type, Token![,]> =
            syn::punctuated::Punctuated::new();
        let mut pred_args: syn::punctuated::Punctuated<syn::FnArg, Token![,]> =
            syn::punctuated::Punctuated::new();
        let mut pred_argnames: syn::punctuated::Punctuated<syn::Ident, Token![,]> =
            syn::punctuated::Punctuated::new();
        let mut pred_params = syn::punctuated::Punctuated::new();
        for (i, ty) in altargty.iter().enumerate() {
            let tr = syn::TypeReference {
                and_token: Token![&](Span::call_site()),
                lifetime: None,
                mutability: None,
                elem: Box::new(ty.clone())
            };
            withfty.push(syn::Type::Reference(tr));
            let mut bounds = syn::punctuated::Punctuated::new();
            let mut segments = syn::punctuated::Punctuated::new();
            let mockall_segment = syn::PathSegment {
                ident: syn::Ident::new("mockall", Span::call_site()),
                arguments: syn::PathArguments::None
            };
            let mut abga_args = syn::punctuated::Punctuated::new();
            for ty in &argty {
                abga_args.push(syn::GenericArgument::Type(ty.clone()));
            }
            let pred_abga = syn::AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: Token![<](Span::call_site()),
                args: abga_args,
                gt_token: Token![>](Span::call_site()),
            };
            let pred_segment = syn::PathSegment {
                ident: syn::Ident::new("Predicate", Span::call_site()),
                arguments: syn::PathArguments::AngleBracketed(pred_abga)
            };
            segments.push(mockall_segment);
            segments.push(pred_segment);
            let leading_colon = Some(Token![::](Span::call_site()));
            let pred_bound = syn::TraitBound {
                paren_token: None,
                modifier: syn::TraitBoundModifier::None,
                lifetimes: None,
                path: syn::Path{leading_colon, segments}
            };
            let mut send_segments = syn::punctuated::Punctuated::new();
            let send_ident = syn::Ident::new("Send", Span::call_site());
            send_segments.push(syn::PathSegment::from(send_ident));
            let send_bound = syn::TraitBound {
                paren_token: None,
                modifier: syn::TraitBoundModifier::None,
                lifetimes: None,
                path: syn::Path{leading_colon: None, segments: send_segments}
            };
            let static_bound = syn::Lifetime::new("'static", Span::call_site());
            bounds.push(syn::TypeParamBound::Trait(pred_bound));
            bounds.push(syn::TypeParamBound::Trait(send_bound));
            bounds.push(syn::TypeParamBound::Lifetime(static_bound));
            let pred_arg_name = syn::Ident::new(&format!("mockall_g{}", i),
                Span::call_site());
            let pred_arg_ty = syn::Ident::new(&format!("MockallG{}", i),
                Span::call_site());
            let tp = syn::TypeParam {
                attrs: vec![],
                ident: pred_arg_ty.clone(),
                colon_token: Some(syn::Token![:](Span::call_site())),
                bounds,
                eq_token: None,
                default: None
            };
            pred_params.push(syn::GenericParam::Type(tp));
            let ps = syn::PathSegment::from(pred_arg_ty);
            let piter = Some(ps).into_iter();
            pred_args.push(syn::FnArg::from(syn::ArgCaptured {
                pat: syn::Pat::from(syn::PatIdent {
                    by_ref: None,
                    mutability: None,
                    ident: pred_arg_name.clone(),
                    subpat: None
                }),
                colon_token: Token![:](Span::call_site()),
                ty: syn::Type::Path(syn::TypePath{
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: syn::punctuated::Punctuated::from_iter(piter)
                    }
                }),
            }));
            pred_argnames.push(pred_arg_name);
        }
        let pred_generics = syn::Generics {
            lt_token: Some(syn::Token![<](Span::call_site())),
            params: pred_params,
            gt_token: Some(syn::Token![>](Span::call_site())),
            where_clause: None
        };
        quote!(
            #attrs
            pub mod #ident {
            ::mockall::static_expectation!{
                #vis [#macro_g] [#argnames] [#argty] [#altargnames]
                [#altargty] [#matchexprs] #output
            }

            /// Like an [`&Expectation`](struct.Expectation.html) but protected
            /// by a Mutex guard.  Useful for mocking static methods.  Forwards
            /// accesses to an `Expectation` object.
            // We must return the MutexGuard to the caller so he can configure
            // the expectation.  But we can't bundle both the guard and the
            // &Expectation into the same structure; the borrow checker won't
            // let us.  Instead we'll record the expectation's position within
            // the Expectations vector so we can proxy its methods.
            //
            // ExpectationGuard is only defined for expectations that return
            // 'static return types.
            #vis struct ExpectationGuard<'guard, #bounded_macro_g>{
                guard: MutexGuard<'guard, Expectations<#macro_g>>,
                i: usize
            }

            impl<'guard, #macro_g> ExpectationGuard<'guard, #macro_g> {
                /// Just like
                /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                #vis fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
                    -> &mut Expectation<#macro_g>
                {
                    self.guard.0[self.i].in_sequence(seq)
                }

                /// Just like
                /// [`Expectation::never`](struct.Expectation.html#method.never)
                #vis fn never(&mut self) -> &mut Expectation<#macro_g> {
                    self.guard.0[self.i].never()
                }

                // Should only be called from the mockall_derive generated code
                #[doc(hidden)]
                #vis fn new(mut guard: MutexGuard<'guard, Expectations<#macro_g>>)
                    -> Self
                {
                    guard.expect(); // Drop the &Expectation
                    let i = guard.0.len() - 1;
                    ExpectationGuard{guard, i}
                }

                /// Just like [`Expectation::once`](struct.Expectation.html#method.once)
                #vis fn once(&mut self) -> &mut Expectation<#macro_g> {
                    self.guard.0[self.i].once()
                }

                /// Just like
                /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                #vis fn returning<F>(&mut self, f: F)
                    -> &mut Expectation<#macro_g>
                    where F: FnMut(#argty) -> #output + Send + 'static
                {
                    self.guard.0[self.i].returning(f)
                }

                /// Just like
                /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                #vis fn return_once<F>(&mut self, f: F)
                    -> &mut Expectation<#macro_g>
                    where F: FnOnce(#argty) -> #output + Send + 'static
                {
                    self.guard.0[self.i].return_once(f)
                }

                /// Just like
                /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                #vis fn returning_st<F>(&mut self, f: F)
                    -> &mut Expectation<#macro_g>
                    where F: FnMut(#argty) -> #output + 'static
                {
                    self.guard.0[self.i].returning_st(f)
                }

                /// Just like
                /// [`Expectation::times`](struct.Expectation.html#method.times)
                #vis fn times(&mut self, n: usize)
                    -> &mut Expectation<#macro_g> {
                    self.guard.0[self.i].times(n)
                }

                /// Just like
                /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                #vis fn times_any(&mut self) -> &mut Expectation<#macro_g> {
                    self.guard.0[self.i].times_any()
                }

                /// Just like
                /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                #vis fn times_range(&mut self, range: Range<usize>)
                    -> &mut Expectation<#macro_g>
                {
                    self.guard.0[self.i].times_range(range)
                }

                /// Just like
                /// [`Expectation::with`](struct.Expectation.html#method.with)
                #vis fn with #pred_generics (&mut self, #pred_args)
                    -> &mut Expectation<#macro_g>
                {
                    self.guard.0[self.i].with(#pred_argnames)
                }

                /// Just like
                /// [`Expectation::withf`](struct.Expectation.html#method.withf)
                #vis fn withf<F>(&mut self, f: F) -> &mut Expectation<#macro_g>
                    where F: Fn(#withfty) -> bool + Send + 'static
                {
                    self.guard.0[self.i].withf(f)
                }
            }

            /// Like a [`ExpectationGuard`](struct.ExpectationGuard.html) but for
            /// generic methods.
            #vis struct GenericExpectationGuard<'guard, #bounded_macro_g> {
                guard: MutexGuard<'guard, GenericExpectations>,
                i: usize,
                _phantom: PhantomData<((), #macro_g)>,
            }

            impl<'guard, #bounded_macro_g>
                GenericExpectationGuard<'guard, #macro_g>
            {
                /// Just like
                /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                #vis fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
                    -> &mut Expectation<#macro_g>
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].in_sequence(seq)
                }

                /// Just like
                /// [`Expectation::never`](struct.Expectation.html#method.never)
                #vis fn never(&mut self) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].never()
                }

                // Should only be called from the mockall_derive generated code
                #[doc(hidden)]
                #vis fn new(mut guard: MutexGuard<'guard, GenericExpectations>)
                    -> Self
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        guard.store.entry(key)
                        .or_insert_with(||
                            Box::new(Expectations::<#macro_g>::new()))
                        .downcast_mut()
                        .unwrap();
                    ee.expect();    // Drop the &Expectation
                    let i = ee.0.len() - 1;
                    GenericExpectationGuard{guard, i, _phantom: PhantomData}
                }

                /// Just like
                /// [`Expectation::once`](struct.Expectation.html#method.once)
                #vis fn once(&mut self) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].once()
                }

                /// Just like
                /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                #vis fn returning<F>(&mut self, f: F) -> &mut Expectation<#macro_g>
                    where F: FnMut(#argty) -> #output + Send + 'static
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].returning(f)
                }

                /// Just like
                /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                #vis fn return_once<F>(&mut self, f: F) -> &mut Expectation<#macro_g>
                    where F: FnOnce(#argty) -> #output + Send + 'static
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].return_once(f)
                }

                /// Just like
                /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                #vis fn returning_st<F>(&mut self, f: F) -> &mut Expectation<#macro_g>
                    where F: FnMut(#argty) -> #output + 'static
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].returning_st(f)
                }

                /// Just like
                /// [`Expectation::times`](struct.Expectation.html#method.times)
                #vis fn times(&mut self, n: usize) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].times(n)
                }

                /// Just like
                /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                #vis fn times_any(&mut self) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].times_any()
                }

                /// Just like
                /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                #vis fn times_range(&mut self, range: Range<usize>) -> &mut Expectation<#macro_g>
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].times_range(range)
                }

                /// Just like
                /// [`Expectation::with`](struct.Expectation.html#method.with)
                #vis fn with #pred_generics (&mut self, #pred_args)
                    -> &mut Expectation<#macro_g>
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].with(#pred_argnames)
                }

                /// Just like
                /// [`Expectation::withf`](struct.Expectation.html#method.withf)
                #vis fn withf<F>(&mut self, f: F) -> &mut Expectation<#macro_g>
                    where F: Fn(#withfty) -> bool + Send + 'static
                {
                    let key = ::mockall::Key::new::<(#argty)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].withf(f)
                }
            }
            }
        )
    } else {
        quote!(
            #attrs
            pub mod #ident {
            ::mockall::static_expectation!{
                #vis [#macro_g] [#argnames] [#argty] [#altargnames]
                [#altargty] [#matchexprs] #output
            }
        })
    }
}
