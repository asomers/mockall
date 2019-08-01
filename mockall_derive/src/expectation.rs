// vim: tw=80
//! A replacement for the old (omnimacro era) expectation!

use super::*;
use std::iter::{FromIterator, IntoIterator};

/// Common methods of all Expectation types
fn common_methods(
    generics: &Punctuated<Ident, Token![,]>,
    argnames: &Punctuated<Pat, Token![,]>,
    altargs: &Punctuated<Pat, Token![,]>,
    matchty: &Punctuated<Type, Token![,]>) -> TokenStream
{
    let static_generics = TokenStream::from_iter(
        generics.iter().map(|g| quote!(#g: 'static, ))
    );
    let args = TokenStream::from_iter(
        argnames.iter().zip(matchty.iter())
        .map(|(argname, mt)| quote!(#argname: &#mt, ))
    );
    let refmatchty = TokenStream::from_iter(
        matchty.iter().map(|mt| quote!(&#mt,))
    );
    let preds = TokenStream::from_iter(
        matchty.iter().map(|mt| quote!(Box<::mockall::Predicate<#mt> + Send>,))
    );
    let pred_matches = TokenStream::from_iter(
        altargs.iter().zip(argnames.iter())
        .map(|(altarg, argname)| quote!(#altarg.eval(#argname),))
    );
    let pred_verify = TokenStream::from_iter(
        altargs.iter().zip(argnames.iter())
        .map(|(altarg, argname)| quote!(
            if let Some(c) = #altarg.find_case(false, #argname){
                panic!("Expectation didn't match arguments:\n{}", c.tree());
            }
        ))
    );
    // TODO: construct new names rather than reuse altargs
    let with_generics = TokenStream::from_iter(
        altargs.iter().zip(matchty.iter())
        .map(|(aa, mt)|
             quote!(#aa: ::mockall::Predicate<#mt> + Send + 'static, )
        )
    );
    let with_args = TokenStream::from_iter(
        argnames.iter().zip(altargs.iter())
        .map(|(argname, altarg)| quote!(#argname: #altarg, ))
    );
    let boxed_withargs = TokenStream::from_iter(
        argnames.iter().map(|aa| quote!(Box::new(#aa), ))
    );
    quote!(
        enum Matcher<#static_generics> {
            Func(Box<Fn(#refmatchty) -> bool + Send>),
            Pred( #preds ),
            // Prevent "unused type parameter" errors
            // Surprisingly, PhantomData<Fn(generics)> is Send even if generics
            // are not, unlike PhantomData<generics>
            _Phantom(Box<Fn(#generics) -> () + Send>)
        }

        impl<#static_generics> Matcher<#generics> {
            fn matches(&self, #args) -> bool {
                match self {
                    Matcher::Func(f) => f(#argnames),
                    Matcher::Pred(#altargs) =>
                        [#pred_matches]
                        .into_iter()
                        .all(|x| *x),
                    _ => unreachable!()
                }
            }

            fn verify(&self, #args ) {
                match self {
                    Matcher::Func(f) => assert!(f(#argnames),
                        "Expectation didn't match arguments"),
                    Matcher::Pred(#altargs) => { #pred_verify },
                    _ => unreachable!()
                }
            }
        }

        impl<#static_generics> Default for Matcher<#generics> {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|#argnames| true))
            }
        }
        /// Holds the stuff that is independent of the output type
        struct Common<#static_generics> {
            matcher: Mutex<Matcher<#generics>>,
            seq_handle: Option<::mockall::SeqHandle>,
            times: ::mockall::Times
        }

        impl<#static_generics> std::default::Default for Common<#generics>
        {
            fn default() -> Self {
                Common {
                    matcher: Mutex::new(Matcher::default()),
                    seq_handle: None,
                    times: ::mockall::Times::default()
                }
            }
        }

        impl<#static_generics> Common<#generics> {
            fn call(&self, #args ) {
                self.matcher.lock().unwrap().verify(#argnames);
                self.times.call();
                self.verify_sequence();
                if self.times.is_satisfied() {
                    self.satisfy_sequence()
                }
            }

            fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
                -> &mut Self
            {
                assert!(self.times.is_exact(),
                    "Only Expectations with an exact call count have sequences");
                self.seq_handle = Some(seq.next());
                self
            }

            fn is_done(&self) -> bool {
                self.times.is_done()
            }

            fn matches(&self, #args) -> bool {
                self.matcher.lock().unwrap().matches(#argnames)
            }

            /// Forbid this expectation from ever being called.
            fn never(&mut self) {
                self.times.never();
            }

            fn satisfy_sequence(&self) {
                if let Some(handle) = &self.seq_handle {
                    handle.satisfy()
                }
            }

            /// Expect this expectation to be called exactly `n` times.
            fn times(&mut self, n: usize) {
                self.times.n(n);
            }

            /// Allow this expectation to be called any number of times
            fn times_any(&mut self) {
                self.times.any();
            }

            /// Allow this expectation to be called any number of times within a
            /// given range
            fn times_range(&mut self, range: Range<usize>) {
                self.times.range(range);
            }

            #[allow(non_camel_case_types)]  // Repurpose $altargs for generics
            fn with<#with_generics>(&mut self, #with_args)
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred(#boxed_withargs);
                mem::replace(guard.deref_mut(), m);
            }

            fn withf<F>(&mut self, f: F)
                where F: Fn(#refmatchty) -> bool + Send + 'static
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Func(Box::new(f));
                mem::replace(guard.deref_mut(), m);
            }

            fn verify_sequence(&self) {
                if let Some(handle) = &self.seq_handle {
                    handle.verify()
                }
            }
        }
    )
}

/// Convert a special reference type like "&str" into a reference to its owned
/// type like "&String".
fn destrify(ty: &mut Type) {
    if let Type::Reference(ref mut tr) = ty {
        let path_ty: TypePath = parse2(quote!(Path)).unwrap();
        let pathbuf_ty: Type = parse2(quote!(::std::path::PathBuf)).unwrap();

        let str_ty: TypePath = parse2(quote!(str)).unwrap();
        let string_ty: Type = parse2(quote!(::std::string::String)).unwrap();

        let cstr_ty: TypePath = parse2(quote!(CStr)).unwrap();
        let cstring_ty: Type = parse2(quote!(::std::ffi::CString)).unwrap();

        let osstr_ty: TypePath = parse2(quote!(OsStr)).unwrap();
        let osstring_ty: Type = parse2(quote!(::std::ffi::OsString)).unwrap();

        match tr.elem.as_ref() {
            Type::Path(ref path) if *path == cstr_ty =>
                *tr.elem = cstring_ty,
            Type::Path(ref path) if *path == osstr_ty =>
                *tr.elem = osstring_ty,
            Type::Path(ref path) if *path == path_ty =>
                *tr.elem = pathbuf_ty,
            Type::Path(ref path) if *path == str_ty =>
                *tr.elem = string_ty,
            _ => (), // Nothing to do
        };
    }
}

fn split_args(args: &Punctuated<FnArg, Token![,]>)
    -> (Punctuated<Pat, Token![,]>,
        Punctuated<Type, Token![,]>)
{
    let mut names = Punctuated::new();
    let mut types = Punctuated::new();
    for fa in args {
        if let FnArg::Captured(ac) = fa {
            names.push(ac.pat.clone());
            types.push(ac.ty.clone());
        }
    }
    (names, types)
}

fn strip_self(args: &Punctuated<FnArg, Token![,]>)
    -> Punctuated<ArgCaptured, Token![,]>
{
    let mut out = Punctuated::new();
    for fa in args {
        if let FnArg::Captured(ac) = fa {
            out.push(ac.clone());
        }
    }
    out
}

/// Generate code for an expectation that returns a 'static value
fn static_expectation(v: &Visibility,
    generics: &Punctuated<Ident, Token![,]>,
    selfless_args: &Punctuated<ArgCaptured, Token![,]>,
    argnames: &Punctuated<Pat, Token![,]>,
    argty: &Punctuated<Type, Token![,]>,
    altargs: &Punctuated<Pat, Token![,]>,
    matchty: &Punctuated<Type, Token![,]>,
    matchcall: &Punctuated<Expr, Token![,]>,
    output: &Type) -> TokenStream
{
    let static_generics = TokenStream::from_iter(
        generics.iter().map(|g| quote!(#g: 'static, ))
    );
    let argnames_nocommas = TokenStream::from_iter(
        argnames.iter().map(|a| quote!(#a))
    );
    let argty_nocommas = TokenStream::from_iter(
        argty.iter().map(|a| quote!(#a))
    );
    let altargs_nocommas = TokenStream::from_iter(
        altargs.iter().map(|a| quote!(#a))
    );
    let matchty_nocommas = TokenStream::from_iter(
        matchty.iter().map(|a| quote!(#a))
    );
    let supersuper_args = Punctuated::<ArgCaptured, Token![,]>::from_iter(
        selfless_args.iter()
        .map(|ac| {
            ArgCaptured {
                pat: ac.pat.clone(),
                colon_token: ac.colon_token.clone(),
                ty: supersuperfy(&ac.ty)
            }
        })
    );
    let cm_ts = common_methods(generics, argnames, altargs, matchty);
    quote!(
        use ::downcast::*;
        use ::fragile::Fragile;
        use ::predicates_tree::CaseTreeExt;
        use ::std::{
            collections::hash_map::HashMap,
            marker::PhantomData,
            mem,
            ops::{DerefMut, Range},
            sync::{Mutex, MutexGuard}
        };
        use super::*;   // Import types from the calling environment

        enum Rfunc<#static_generics> {
            Default,
            // Indicates that a `return_once` expectation has already returned
            Expired,
            Mut(Box<dyn FnMut(#argty) -> #output + Send>),
            // Should be Box<dyn FnOnce> once that feature is stabilized
            // https://github.com/rust-lang/rust/issues/28796
            Once(Box<dyn FnMut(#argty) -> #output + Send>),
            // Prevent "unused type parameter" errors
            // Surprisingly, PhantomData<Fn(generics)> is Send even if generics
            // are not, unlike PhantomData<generics>
            _Phantom(Box<Fn(#generics) -> () + Send>)
        }

        impl<#generics>  Rfunc<#generics> {
            fn call_mut(&mut self, #supersuper_args ) -> #output {
                match self {
                    Rfunc::Default => {
                        use ::mockall::ReturnDefault;
                        ::mockall::DefaultReturner::<#output>::return_default()
                    },
                    Rfunc::Expired => {
                        panic!("Called a method twice that was expected only once")
                    },
                    Rfunc::Mut(f) => {
                        f( #argnames )
                    },
                    Rfunc::Once(_) => {
                        if let Rfunc::Once(mut f) =
                            mem::replace(self, Rfunc::Expired) {
                            f( #argnames )
                        } else {
                            unreachable!()
                        }
                    },
                    Rfunc::_Phantom(_) => unreachable!()
                }
            }
        }

        impl<#generics>
            std::default::Default for Rfunc<#generics>
        {
            fn default() -> Self {
                Rfunc::Default
            }
        }

        #cm_ts

        /// Expectation type for methods that return a `'static` type.
        /// This is the type returned by the `expect_*` methods.
        #v struct Expectation<#static_generics> {
            common: Common<#generics>,
            rfunc: Mutex<Rfunc<#generics>>,
        }

        impl<#generics> Expectation<#generics> {
            /// Call this [`Expectation`] as if it were the real method.
            #[doc(hidden)]
            #v fn call(&self, #supersuper_args ) -> #output
            {
                self.common.call(#matchcall);
                self.rfunc.lock().unwrap().call_mut(#argnames)
            }

            /// Return a constant value from the `Expectation`
            ///
            /// The output type must be `Clone`.  The compiler can't always
            /// infer the proper type to use with this method; you will usually
            /// need to specify it explicitly.  i.e. `return_const(42i32)`
            /// instead of `return_const(42)`.
            // We must use Into<#output> instead of #output because where
            // clauses don't accept equality constraints.
            // https://github.com/rust-lang/rust/issues/20041
            #[allow(unused_variables)]
            #v fn return_const<MockallOutput>(&mut self, c: MockallOutput)
                -> &mut Self
                where MockallOutput: Clone + Into<#output> + Send + 'static
            {
                self.returning(move |#supersuper_args| c.clone().into())
            }

            /// Supply an `FnOnce` closure that will provide the return value
            /// for this Expectation.  This is useful for return types that
            /// aren't `Clone`.  It will be an error to call this method
            /// multiple times.
            #v fn return_once<F>(&mut self, f: F) -> &mut Self
                where F: FnOnce(#argty) -> #output + Send + 'static
            {
                let mut fopt = Some(f);
                let fmut = move |#supersuper_args| {
                    if let Some(f) = fopt.take() {
                        f(#argnames)
                    } else {
                        panic!("Called a method twice that was expected only once")
                    }
                };
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(),
                                 Rfunc::Once(Box::new(fmut)));
                }
                self
            }

            /// Single-threaded version of [`return_once`](#method.return_once).
            /// This is useful for return types that are neither `Send` nor
            /// `Clone`.
            ///
            /// It is a runtime error to call the mock method from a different
            /// thread than the one that originally called this method.  It is
            /// also a runtime error to call the method more than once.
            #v fn return_once_st<F>(&mut self, f: F) -> &mut Self
                where F: FnOnce(#argty) -> #output + 'static
            {
                let mut fragile = Some(::fragile::Fragile::new(f));
                let fmut = Box::new(move |#supersuper_args| {
                    match fragile.take() {
                        Some(frag) => (frag.into_inner())(#argnames),
                        None => panic!(
                            "Called a method twice that was expected only once")
                    }
                });
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(), Rfunc::Once(fmut));
                }
                self
            }

            /// Supply a closure that will provide the return value for this
            /// `Expectation`.  The method's arguments are passed to the closure
            /// by value.
            #v fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut(#argty) -> #output + Send + 'static
            {
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
                }
                self
            }

            /// Single-threaded version of [`returning`](#method.returning).
            /// Can be used when the argument or return type isn't `Send`.
            ///
            /// It is a runtime error to call the mock method from a different
            /// thread than the one that originally called this method.
            #v fn returning_st<F>(&mut self, f: F) -> &mut Self
                where F: FnMut(#argty) -> #output + 'static
            {
                let mut fragile = Fragile::new(f);
                let fmut = move |#supersuper_args| {
                    (fragile.get_mut())(#argnames)
                };
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(fmut)));
                }
                self
            }

            ::mockall::expectation_methods!{
                #v [#argnames_nocommas] [#altargs_nocommas] [#matchty_nocommas]
            }
        }
        impl<#generics> Default for Expectation<#generics>
        {
            fn default() -> Self {
                Expectation {
                    common: Common::default(),
                    rfunc: Mutex::new(Rfunc::default())
                }
            }
        }

        ::mockall::expectations_methods!{#v [#generics]}
        impl<#generics> Expectations<#generics> {
            /// Simulating calling the real method.  Every current expectation
            /// will be checked in FIFO order and the first one with matching
            /// arguments will be used.
            #v fn call(&self, #supersuper_args ) -> #output
            {
                match self.0.iter()
                    .find(|e| e.matches(#matchcall) &&
                          (!e.is_done() || self.0.len() == 1))
                {
                    None => panic!("No matching expectation found"),
                    Some(e) => e.call(#argnames)
                }
            }

        }
        impl<#static_generics>
            ::mockall::AnyExpectations for Expectations<#generics>
        {}

        ::mockall::generic_expectation_methods!{#v [#generics] [#argty_nocommas]
            #output
        }
        impl GenericExpectations {
            /// Simulating calling the real method.
            #v fn call<#static_generics>
                (&self, #supersuper_args ) -> #output
            {
                self.store.get(&::mockall::Key::new::<(#argty)>())
                    .expect("No matching expectation found")
                    .downcast_ref::<Expectations<#generics>>()
                    .unwrap()
                    .call(#argnames)
            }

            /// Create a new Expectation.
            #v fn expect<'e, #static_generics>
                (&'e mut self)
                -> &'e mut Expectation<#generics>
            {
                self.store.entry(::mockall::Key::new::<(#argty)>())
                    .or_insert_with(||
                        Box::new(Expectations::<#generics>::new())
                    ).downcast_mut::<Expectations<#generics>>()
                    .unwrap()
                    .expect()
            }

        }
    )
}

/// Generate the code that implements an expectation for a single method
pub(crate) fn expectation(attrs: &TokenStream, vis: &Visibility,
    self_ident: Option<&Ident>,
    ident: &Ident,
    generics: &Generics,
    args: &Punctuated<FnArg, Token![,]>,
    return_type: &ReturnType,
    altargs: &Punctuated<FnArg, Token![,]>,
    matchexprs: &Punctuated<Expr, Token![,]>
    ) -> TokenStream
{
    let mut ref_expectation = false;
    let mut ref_mut_expectation = false;
    let mut static_method = true;

    let static_bound = Lifetime::new("'static", Span::call_site());

    let mut rt2 = return_type.clone();
    let output = match rt2 {
        ReturnType::Default => Box::new(Type::Tuple(TypeTuple {
            paren_token: token::Paren::default(),
            elems: Punctuated::new()
        })),
        ReturnType::Type(_, ref mut ty) => {
            let mut rt = ty.clone();
            destrify(&mut rt);
            if let Some(i) = self_ident {
                crate::deselfify(&mut rt, i);
            }
            if let Type::Reference(ref tr) = rt.as_ref() {
                if tr.lifetime.as_ref().map_or(false, |lt| lt.ident == "static")
                {
                    // Just a static expectation
                    rt
                } else {
                    if !tr.mutability.is_some() {
                        ref_expectation = true;
                    } else {
                        ref_mut_expectation = true;
                    }
                    tr.elem.clone()
                }
            } else {
                rt
            }
        }
    };
    let supersuper_output = supersuperfy(&output);
    let output = quote!(#output);

    let mut egenerics = generics.clone();
    let mut macro_g = Punctuated::<Ident, Token![,]>::new();
    // Convert the TypeGenerics into a sequence of idents, because that's
    // what static_expectation! expects
    // TODO: once static_expectation! is gone, something like this:
    //generics.split_for_impl().1.to_tokens(&mut macro_g)
    for p in egenerics.params.iter_mut() {
        if let GenericParam::Type(tp) = p {
            tp.bounds.push(TypeParamBound::Lifetime(static_bound.clone()));
        }
    }
    let (bounded_macro_g, _, _) = egenerics.split_for_impl();
    for p in generics.params.iter() {
        if let GenericParam::Type(tp) = p {
            macro_g.push(tp.ident.clone());
        }
    }
    if !macro_g.empty_or_trailing() {
        // static_expectation! requires trailing punctuation
        macro_g.push_punct(Token![,](Span::call_site()));
    }
    let mut eltgenerics = egenerics.clone();
    let ltdef = LifetimeDef::new(Lifetime::new("'lt", Span::call_site()));
    eltgenerics.params.push(GenericParam::Lifetime(ltdef));
    let (bounded_lt_macro_g, _, _) = eltgenerics.split_for_impl();
    for fa in args {
        static_method &= match fa {
            FnArg::SelfRef(_) | FnArg::SelfValue(_) => false,
            _ => true
        };
    }

    let selfless_args = strip_self(args);
    let (argnames, argty) = split_args(args);
    let argnames_nocommas = TokenStream::from_iter(
        argnames.iter().map(|a| quote!(#a))
    );
    let argty_nocommas = TokenStream::from_iter(
        argty.iter().map(|a| quote!(#a))
    );
    let mut supersuper_argty = Punctuated::<Type, token::Comma>::from_iter(
        argty.iter()
        .map(|t| supersuperfy(&t))
    );
    if !supersuper_argty.empty_or_trailing() {
        // The non-proc macro static_expectation! always uses trailing
        // punctuation.  Until we eliminate that macro, the proc macro must use
        // trailing punctuation to match.
        supersuper_argty.push_punct(Token![,](Span::call_site()));
    }
    let mut argty_tp = argty.clone();
    if !argty_tp.empty_or_trailing() {
        // The non-proc macro static_expectation! always uses trailing
        // punctuation.  Until we eliminate that macro, the proc macro must use
        // trailing punctuation to match.
        argty_tp.push_punct(Token![,](Span::call_site()));
    }
    let (altargnames, altargty) = split_args(altargs);
    let altargnames_nocommas = TokenStream::from_iter(
        altargnames.iter().map(|a| quote!(#a))
    );
    let altargty_nocommas = TokenStream::from_iter(
        altargty.iter().map(|a| quote!(#a))
    );
    let supersuper_altargty = Punctuated::<Type, token::Comma>::from_iter(
        altargty.iter()
        .map(|t| supersuperfy(&t))
    );
    let cm_ts = common_methods(&macro_g, &argnames, &altargnames, &altargty);
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

            #cm_ts

            /// Expectation type for methods taking a `&self` argument and
            /// returning immutable references.  This is the type returned by
            /// the `expect_*` methods.
            #vis struct Expectation #bounded_macro_g {
                common: Common<#macro_g>,
                result: Option<#output>,
            }

            impl #bounded_macro_g Expectation<#macro_g> {
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
                    #vis [#argnames_nocommas] [#altargnames_nocommas]
                    [#altargty_nocommas]
                }
            }

            impl #bounded_macro_g Default for Expectation<#macro_g>
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        result: None
                    }
                }
            }

            ::mockall::expectations_methods!{#vis [#macro_g]}
            impl #bounded_macro_g Expectations<#macro_g> {
                /// Simulating calling the real method.  Every current
                /// expectation will be checked in FIFO order and the first one
                /// with matching arguments will be used.
                #vis fn call(&self, #selfless_args ) -> &#output {
                    match self.0.iter()
                        .find(|e| e.matches(#matchexprs) &&
                              (!e.is_done() || self.0.len() == 1))
                    {
                        None => panic!("No matching expectation found"),
                        Some(__e) => __e.call(#argnames)
                    }
                }
            }
            // The Senc + Sync are required for downcast, since Expectation
            // stores an Option<#output>
            impl #bounded_macro_g
                ::mockall::AnyExpectations for Expectations<#macro_g>
                    where #output: Send + Sync
            {}

            ::mockall::generic_expectation_methods!{
                #vis [#macro_g] [#argty_nocommas] #supersuper_output
            }
            impl GenericExpectations {
                /// Simulating calling the real method.
                #vis fn call #bounded_macro_g
                    (&self, #selfless_args ) -> &#output
                {
                    self.store.get(&::mockall::Key::new::<(#argty_tp)>())
                        .expect("No matching expectation found")
                        .downcast_ref::<Expectations<#macro_g>>()
                        .unwrap()
                        .call(#argnames)
                }

                /// Create a new Expectation.
                #vis fn expect #bounded_lt_macro_g (&'lt mut self)
                    -> &'lt mut Expectation<#macro_g>
                    where #output: Send + Sync
                {
                    self.store.entry(::mockall::Key::new::<(#argty_tp)>())
                        .or_insert_with(||
                            Box::new(Expectations::<#macro_g>::new())
                        ).downcast_mut::<Expectations<#macro_g>>()
                        .unwrap()
                        .expect()
                }
            }
        }
        )
    } else if ref_mut_expectation {
        let cm_ts = common_methods(&macro_g, &argnames, &altargnames,
                                   &altargty);
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

            #cm_ts

            /// Expectation type for methods taking a `&mut self` argument and
            /// returning references.  This is the type returned by the
            /// `expect_*` methods.
            #vis struct Expectation #bounded_macro_g {
                common: Common<#macro_g>,
                result: Option<#output>,
                rfunc: Option<Box<dyn FnMut(#argty) -> #output + Send + Sync>>,
            }

            impl #bounded_macro_g Expectation<#macro_g> {
                /// Simulating calling the real method for this expectation
                #vis fn call_mut(&mut self, #selfless_args) -> &mut #output {
                    self.common.call(#matchexprs);
                    if let Some(ref mut __f) = self.rfunc {
                        self.result = Some(__f(#argnames));
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
                    let mut __fragile = Fragile::new(f);
                    let __fmut = move |#selfless_args| {
                        (__fragile.get_mut())(#argnames)
                    };
                    mem::replace(&mut self.rfunc, Some(Box::new(__fmut)));
                    self
                }

                ::mockall::expectation_methods!{
                    #vis [#argnames_nocommas] [#altargnames_nocommas]
                    [#altargty_nocommas]
                }
            }
            impl #bounded_macro_g Default for Expectation<#macro_g>
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
            impl #bounded_macro_g Expectations<#macro_g> {
                /// Simulating calling the real method.  Every current
                /// expectation will be checked in FIFO order and the first one
                /// with matching arguments will be used.
                #vis fn call_mut(&mut self, #selfless_args ) -> &mut #output {
                    let __n = self.0.len();
                    match self.0.iter_mut()
                        .find(|e| e.matches(#matchexprs) &&
                              (!e.is_done() || __n == 1))
                    {
                        None => panic!("No matching expectation found"),
                        Some(__e) => __e.call_mut(#argnames)
                    }
                }
            }
            // The Senc + Sync are required for downcast, since Expectation
            // stores an Option<#output>
            impl #bounded_macro_g
                ::mockall::AnyExpectations for Expectations<#macro_g>
                where #output: Send + Sync
            {}

            ::mockall::generic_expectation_methods!{
                #vis [#macro_g] [#argty_nocommas] #output
            }
            impl GenericExpectations {
                /// Simulating calling the real method.
                #vis fn call_mut #bounded_macro_g
                    (&mut self, #selfless_args ) -> &mut #output
                {
                    self.store.get_mut(&::mockall::Key::new::<(#argty_tp)>())
                        .expect("No matching expectation found")
                        .downcast_mut::<Expectations<#macro_g>>()
                        .unwrap()
                        .call_mut(#argnames)
                }

                /// Create a new Expectation.
                #vis fn expect #bounded_lt_macro_g (&'lt mut self)
                    -> &'lt mut Expectation<#macro_g>
                    where #output: Send + Sync
                {
                    self.store.entry(::mockall::Key::new::<(#argty_tp)>())
                        .or_insert_with(||
                            Box::new(Expectations::<#macro_g>::new())
                        ).downcast_mut::<Expectations<#macro_g>>()
                        .unwrap()
                        .expect()
                }
            }
        }
        )
    } else if static_method {
        let mut withfty: Punctuated<Type, Token![,]> = Punctuated::new();
        let mut pred_args: Punctuated<FnArg, Token![,]> = Punctuated::new();
        let mut pred_argnames: Punctuated<Ident, Token![,]> = Punctuated::new();
        let mut pred_params = Punctuated::new();
        for (i, ty) in altargty.iter().enumerate() {
            let tr = TypeReference {
                and_token: Token![&](Span::call_site()),
                lifetime: None,
                mutability: None,
                elem: Box::new(ty.clone())
            };
            withfty.push(Type::Reference(tr));
            let mut bounds = Punctuated::new();
            let mut segments = Punctuated::new();
            let mockall_segment = PathSegment {
                ident: Ident::new("mockall", Span::call_site()),
                arguments: PathArguments::None
            };
            let mut abga_args = Punctuated::new();
            for ty in &argty {
                abga_args.push(GenericArgument::Type(ty.clone()));
            }
            let pred_abga = AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: Token![<](Span::call_site()),
                args: abga_args,
                gt_token: Token![>](Span::call_site()),
            };
            let pred_segment = PathSegment {
                ident: Ident::new("Predicate", Span::call_site()),
                arguments: PathArguments::AngleBracketed(pred_abga)
            };
            segments.push(mockall_segment);
            segments.push(pred_segment);
            let leading_colon = Some(Token![::](Span::call_site()));
            let pred_bound = TraitBound {
                paren_token: None,
                modifier: TraitBoundModifier::None,
                lifetimes: None,
                path: Path{leading_colon, segments}
            };
            let mut send_segments = Punctuated::new();
            let send_ident = Ident::new("Send", Span::call_site());
            send_segments.push(PathSegment::from(send_ident));
            let send_bound = TraitBound {
                paren_token: None,
                modifier: TraitBoundModifier::None,
                lifetimes: None,
                path: Path{leading_colon: None, segments: send_segments}
            };
            bounds.push(TypeParamBound::Trait(pred_bound));
            bounds.push(TypeParamBound::Trait(send_bound));
            bounds.push(TypeParamBound::Lifetime(static_bound.clone()));
            let pred_arg_name = Ident::new(&format!("mockall_g{}", i),
                Span::call_site());
            let pred_arg_ty = Ident::new(&format!("MockallG{}", i),
                Span::call_site());
            let tp = TypeParam {
                attrs: vec![],
                ident: pred_arg_ty.clone(),
                colon_token: Some(Token![:](Span::call_site())),
                bounds,
                eq_token: None,
                default: None
            };
            pred_params.push(GenericParam::Type(tp));
            let ps = PathSegment::from(pred_arg_ty);
            let piter = Some(ps).into_iter();
            pred_args.push(FnArg::from(ArgCaptured {
                pat: Pat::from(PatIdent {
                    by_ref: None,
                    mutability: None,
                    ident: pred_arg_name.clone(),
                    subpat: None
                }),
                colon_token: Token![:](Span::call_site()),
                ty: Type::Path(TypePath{
                    qself: None,
                    path: Path {
                        leading_colon: None,
                        segments: Punctuated::from_iter(piter)
                    }
                }),
            }));
            pred_argnames.push(pred_arg_name);
        }
        let pred_generics = Generics {
            lt_token: Some(Token![<](Span::call_site())),
            params: pred_params,
            gt_token: Some(Token![>](Span::call_site())),
            where_clause: None
        };
        let ts = static_expectation(vis, &macro_g, &selfless_args, &argnames,
                                    &supersuper_argty, &altargnames,
                                    &supersuper_altargty, &matchexprs,
                                    &supersuper_output);
        quote!(
            #attrs
            pub mod #ident {
            #ts

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
            #vis struct ExpectationGuard #bounded_lt_macro_g {
                guard: MutexGuard<'lt, Expectations<#macro_g>>,
                i: usize
            }

            impl #bounded_lt_macro_g ExpectationGuard<'lt, #macro_g> {
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
                #vis fn new(mut guard: MutexGuard<'lt, Expectations<#macro_g>>)
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
            #vis struct GenericExpectationGuard #bounded_lt_macro_g {
                guard: MutexGuard<'lt, GenericExpectations>,
                i: usize,
                _phantom: PhantomData<((), #macro_g)>,
            }

            impl #bounded_lt_macro_g
                GenericExpectationGuard<'lt, #macro_g>
            {
                /// Just like
                /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                #vis fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
                    -> &mut Expectation<#macro_g>
                {
                    let key = ::mockall::Key::new::<(#argty_tp)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].in_sequence(seq)
                }

                /// Just like
                /// [`Expectation::never`](struct.Expectation.html#method.never)
                #vis fn never(&mut self) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty_tp)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].never()
                }

                // Should only be called from the mockall_derive generated code
                #[doc(hidden)]
                #vis fn new(mut guard: MutexGuard<'lt, GenericExpectations>)
                    -> Self
                {
                    let key = ::mockall::Key::new::<(#argty_tp)>();
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
                    let key = ::mockall::Key::new::<(#argty_tp)>();
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
                    let key = ::mockall::Key::new::<(#argty_tp)>();
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
                    let key = ::mockall::Key::new::<(#argty_tp)>();
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
                    let key = ::mockall::Key::new::<(#argty_tp)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].returning_st(f)
                }

                /// Just like
                /// [`Expectation::times`](struct.Expectation.html#method.times)
                #vis fn times(&mut self, n: usize) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty_tp)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].times(n)
                }

                /// Just like
                /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                #vis fn times_any(&mut self) -> &mut Expectation<#macro_g> {
                    let key = ::mockall::Key::new::<(#argty_tp)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].times_any()
                }

                /// Just like
                /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                #vis fn times_range(&mut self, range: Range<usize>) -> &mut Expectation<#macro_g>
                {
                    let key = ::mockall::Key::new::<(#argty_tp)>();
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
                    let key = ::mockall::Key::new::<(#argty_tp)>();
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
                    let key = ::mockall::Key::new::<(#argty_tp)>();
                    let ee: &mut Expectations<#macro_g> =
                        self.guard.store.get_mut(&key).unwrap()
                        .downcast_mut().unwrap();
                    ee.0[self.i].withf(f)
                }
            }
            }
        )
    } else {
        let ts = static_expectation(vis, &macro_g, &selfless_args, &argnames,
                                    &supersuper_argty, &altargnames,
                                    &supersuper_altargty, &matchexprs,
                                    &supersuper_output);
        quote!(
            #attrs
            pub mod #ident { #ts }
        )
    }
}
