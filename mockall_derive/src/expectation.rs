// vim: tw=80
//! A replacement for the old (omnimacro era) expectation!

use super::*;
use std::iter::{FromIterator, IntoIterator};

/// Common methods of all Expectation types
fn common_methods(
    egenerics: &Generics,
    argnames: &Punctuated<Pat, Token![,]>,
    matchty: &Punctuated<Type, Token![,]>) -> TokenStream
{
    let (egenerics_ig, egenerics_tg, egenerics_wc) = egenerics.split_for_impl();
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
        argnames.iter().enumerate()
        .map(|(i, argname)| {
            let idx = syn::Index::from(i);
            quote!(__pred.#idx.eval(#argname),)
        })
    );
    let pred_verify = TokenStream::from_iter(
        argnames.iter().enumerate()
        .map(|(i, argname)| {
            let idx = Index::from(i);
            quote!(
                if let Some(c) = __pred.#idx.find_case(false, #argname){
                    panic!("Expectation didn't match arguments:\n{}", c.tree());
                }
            )
        })
    );
    let with_generics_idents = (0..matchty.len())
        .map(|i| Ident::new(&format!("__Matcher{}", i), Span::call_site()))
        .collect::<Vec<_>>();
    let with_generics = TokenStream::from_iter(
        with_generics_idents.iter().zip(matchty.iter())
        .map(|(id, mt)|
            quote!(#id: ::mockall::Predicate<#mt> + Send + 'static, )
        )
    );
    let with_args = TokenStream::from_iter(
        argnames.iter().zip(with_generics_idents.iter())
        .map(|(argname, id)| quote!(#argname: #id, ))
    );
    let boxed_withargs = TokenStream::from_iter(
        argnames.iter().map(|aa| quote!(Box::new(#aa), ))
    );
    let fn_params = Punctuated::<Ident, Token![,]>::from_iter(
        egenerics.type_params().map(|tp| tp.ident.clone())
    );
    quote!(
        enum Matcher #egenerics_ig #egenerics_wc {
            Func(Box<Fn(#refmatchty) -> bool + Send>),
            Pred(Box<(#preds)>),
            // Prevent "unused type parameter" errors
            // Surprisingly, PhantomData<Fn(generics)> is Send even if generics
            // are not, unlike PhantomData<generics>
            _Phantom(Box<Fn(#fn_params) -> () + Send>)
        }

        impl #egenerics_ig Matcher #egenerics_tg #egenerics_wc {
            fn matches(&self, #args) -> bool {
                match self {
                    Matcher::Func(f) => f(#argnames),
                    Matcher::Pred(__pred) =>
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
                    Matcher::Pred(__pred) => { #pred_verify },
                    _ => unreachable!()
                }
            }
        }

        impl #egenerics_ig Default for Matcher #egenerics_tg #egenerics_wc {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|#argnames| true))
            }
        }
        /// Holds the stuff that is independent of the output type
        struct Common #egenerics_ig #egenerics_wc {
            matcher: Mutex<Matcher #egenerics_tg>,
            seq_handle: Option<::mockall::SeqHandle>,
            times: ::mockall::Times
        }

        impl #egenerics_ig std::default::Default for Common #egenerics_tg
            #egenerics_wc
        {
            fn default() -> Self {
                Common {
                    matcher: Mutex::new(Matcher::default()),
                    seq_handle: None,
                    times: ::mockall::Times::default()
                }
            }
        }

        impl #egenerics_ig Common #egenerics_tg #egenerics_wc {
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
                self.seq_handle = Some(seq.next_handle());
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

            fn with<#with_generics>(&mut self, #with_args)
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred(Box::new((#boxed_withargs)));
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

/// Methods of the Expectation structs that are common for static expectations,
/// ref expectations, and ref mut expectations
fn expectation_methods(v: &Visibility,
    argnames: &Punctuated<Pat, Token![,]>,
    matchty: &Punctuated<Type, Token![,]>) -> TokenStream
{
    let args = TokenStream::from_iter(
        argnames.iter().zip(matchty.iter())
        .map(|(argname, mt)| quote!(#argname: &#mt, ))
    );
    let with_generics_idents = (0..matchty.len())
        .map(|i| Ident::new(&format!("__Matcher{}", i), Span::call_site()))
        .collect::<Vec<_>>();
    let with_generics = TokenStream::from_iter(
        with_generics_idents.iter().zip(matchty.iter())
        .map(|(id, mt)|
            quote!(#id: ::mockall::Predicate<#mt> + Send + 'static, )
        )
    );
    let with_args = TokenStream::from_iter(
        argnames.iter().zip(with_generics_idents.iter())
        .map(|(argname, id)| quote!(#argname: #id, ))
    );
    let refmatchty = TokenStream::from_iter(
        matchty.iter().map(|mt| quote!(&#mt,))
    );
    quote!(
        /// Add this expectation to a
        /// [`Sequence`](../../../mockall/struct.Sequence.html).
        #v fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
            -> &mut Self
        {
            self.common.in_sequence(seq);
            self
        }

        fn is_done(&self) -> bool {
            self.common.is_done()
        }

        /// Validate this expectation's matcher.
        fn matches(&self, #args) -> bool {
            self.common.matches(#argnames)
        }

        /// Forbid this expectation from ever being called.
        #v fn never(&mut self) -> &mut Self {
            self.common.never();
            self
        }

        /// Create a new, default, [`Expectation`](struct.Expectation.html)
        #v fn new() -> Self {
            Self::default()
        }

        /// Expect this expectation to be called exactly once.  Shortcut for
        /// [`times(1)`](#method.times).
        #v fn once(&mut self) -> &mut Self {
            self.times(1)
        }

        /// Expect this expectation to be called exactly `n` times.
        #v fn times(&mut self, n: usize) -> &mut Self {
            self.common.times(n);
            self
        }

        /// Allow this expectation to be called any number of times
        ///
        /// This behavior is the default, but the method is provided in case the
        /// default behavior changes.
        #v fn times_any(&mut self) -> &mut Self {
            self.common.times_any();
            self
        }

        /// Allow this expectation to be called any number of times within a
        /// given range
        #v fn times_range(&mut self, range: Range<usize>)
            -> &mut Self
        {
            self.common.times_range(range);
            self
        }

        /// Set matching crieteria for this Expectation.
        ///
        /// The matching predicate can be anything implemening the
        /// [`Predicate`](../../../mockall/trait.Predicate.html) trait.  Only
        /// one matcher can be set per `Expectation` at a time.
        #v fn with<#with_generics>(&mut self, #with_args) -> &mut Self
        {
            self.common.with(#argnames);
            self
        }

        /// Set a matching function for this Expectation.
        ///
        /// This is equivalent to calling [`with`](#method.with) with a function
        /// argument, like `with(predicate::function(f))`.
        #v fn withf<F>(&mut self, f: F) -> &mut Self
            where F: Fn(#refmatchty) -> bool + Send + 'static
        {
            self.common.withf(f);
            self
        }
    )
}

/// Common methods of the Expectations structs
fn expectations_methods(v: &Visibility,
    egenerics: &Generics) -> TokenStream
{
    let (egenerics_ig, egenerics_tg, egenerics_wc) = egenerics.split_for_impl();
    quote!(
        /// A collection of [`Expectation`](struct.Expectations.html) objects.
        /// Users will rarely if ever use this struct directly.
        #[doc(hidden)]
        #v struct Expectations #egenerics_ig ( Vec<Expectation #egenerics_tg>)
        #egenerics_wc;

        impl #egenerics_ig Expectations #egenerics_tg #egenerics_wc {
            /// Verify that all current expectations are satisfied and clear
            /// them.
            #v fn checkpoint(&mut self) {
                self.0.drain(..);
            }

            /// Create a new expectation for this method.
            #v fn expect(&mut self) -> &mut Expectation #egenerics_tg
            {
                let e = Expectation::default();
                self.0.push(e);
                let l = self.0.len();
                &mut self.0[l - 1]
            }

            #v fn new() -> Self {
                Self::default()
            }
        }
        impl #egenerics_ig Default for Expectations #egenerics_tg #egenerics_wc
        {
            fn default() -> Self {
                Expectations(Vec::new())
            }
        }
    )
}

fn generic_expectation_methods(v: &Visibility) -> TokenStream
{
    quote!(
        /// A collection of [`Expectation`](struct.Expectations.html) objects
        /// for a generic method.  Users will rarely if ever use this struct
        /// directly.
        #[doc(hidden)]
        #[derive(Default)]
        #v struct GenericExpectations{
            store: HashMap<::mockall::Key, Box<dyn ::mockall::AnyExpectations>>
        }
        impl GenericExpectations {
            /// Verify that all current expectations are satisfied and clear
            /// them.  This applies to all sets of generic parameters!
            #v fn checkpoint(&mut self) {
                self.store.clear();
            }

            #v fn new() -> Self {
                Self::default()
            }
        }
    )
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
///
/// # Arguments
/// * `egenerics` - The method's generic parameters, with `'static` bounds added
fn static_expectation(v: &Visibility,
    egenerics: &Generics,
    selfless_args: &Punctuated<ArgCaptured, Token![,]>,
    argnames: &Punctuated<Pat, Token![,]>,
    argty: &Punctuated<Type, Token![,]>,
    matchty: &Punctuated<Type, Token![,]>,
    matchcall: &Punctuated<Expr, Token![,]>,
    output: &Type) -> TokenStream
{
    let (egenerics_ig, egenerics_tg, egenerics_wc) = egenerics.split_for_impl();
    let egenerics_tbf = egenerics_tg.as_turbofish();
    let supersuper_args = Punctuated::<ArgCaptured, Token![,]>::from_iter(
        selfless_args.iter()
        .map(|ac| {
            ArgCaptured {
                pat: ac.pat.clone(),
                colon_token: ac.colon_token,
                ty: supersuperfy(&ac.ty)
            }
        })
    );
    let cm_ts = common_methods(egenerics, argnames, matchty);
    let em_ts = expectation_methods(v, argnames, matchty);
    let eem_ts = expectations_methods(v, egenerics);
    let gem_ts = generic_expectation_methods(v);
    let fn_params = Punctuated::<Ident, Token![,]>::from_iter(
        egenerics.type_params().map(|tp| tp.ident.clone())
    );
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

        enum Rfunc #egenerics_ig #egenerics_wc {
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
            _Phantom(Box<Fn(#fn_params) -> () + Send>)
        }

        impl #egenerics_ig  Rfunc #egenerics_tg #egenerics_wc {
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

        impl #egenerics_ig
            std::default::Default for Rfunc #egenerics_tg #egenerics_wc
        {
            fn default() -> Self {
                Rfunc::Default
            }
        }

        #cm_ts

        /// Expectation type for methods that return a `'static` type.
        /// This is the type returned by the `expect_*` methods.
        #v struct Expectation #egenerics_ig #egenerics_wc {
            common: Common #egenerics_tg,
            rfunc: Mutex<Rfunc #egenerics_tg>,
        }

        impl #egenerics_ig Expectation #egenerics_tg #egenerics_wc {
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

            #em_ts
        }
        impl #egenerics_ig Default for Expectation #egenerics_tg #egenerics_wc
        {
            fn default() -> Self {
                Expectation {
                    common: Common::default(),
                    rfunc: Mutex::new(Rfunc::default())
                }
            }
        }

        #eem_ts
        impl #egenerics_ig Expectations #egenerics_tg #egenerics_wc {
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
        impl #egenerics_ig ::mockall::AnyExpectations
            for Expectations #egenerics_tg #egenerics_wc
        {}

        #gem_ts
        impl GenericExpectations {
            /// Simulating calling the real method.
            #v fn call #egenerics_ig (&self, #supersuper_args ) -> #output
                #egenerics_wc
            {
                self.store.get(&::mockall::Key::new::<(#argty)>())
                    .expect("No matching expectation found")
                    .downcast_ref::<Expectations #egenerics_tg>()
                    .unwrap()
                    .call(#argnames)
            }

            /// Create a new Expectation.
            #v fn expect #egenerics_ig (&mut self)
                -> &mut Expectation #egenerics_tg
                #egenerics_wc
            {
                self.store.entry(::mockall::Key::new::<(#argty)>())
                    .or_insert_with(||
                        Box::new(Expectations #egenerics_tbf::new())
                    ).downcast_mut::<Expectations #egenerics_tg>()
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
    altargty: &Punctuated<Type, Token![,]>,
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
                    if tr.mutability.is_none() {
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
    for p in egenerics.params.iter_mut() {
        if let GenericParam::Type(tp) = p {
            tp.bounds.push(TypeParamBound::Lifetime(static_bound.clone()));
        }
    }
    let (egenerics_ig, egenerics_tg, _) = egenerics.split_for_impl();
    let egenerics_tbf = egenerics_tg.as_turbofish();
    let mut eltgenerics = egenerics.clone();
    let ltdef = LifetimeDef::new(Lifetime::new("'lt", Span::call_site()));
    eltgenerics.params.push(GenericParam::Lifetime(ltdef));
    let (eltgenerics_ig, eltgenerics_tg, eltgenerics_wc) =
        eltgenerics.split_for_impl();
    let fn_params = Punctuated::<Ident, Token![,]>::from_iter(
        egenerics.type_params().map(|tp| tp.ident.clone())
    );
    for fa in args {
        static_method &= match fa {
            FnArg::SelfRef(_) | FnArg::SelfValue(_) => false,
            _ => true
        };
    }

    let selfless_args = strip_self(args);
    let (argnames, argty) = split_args(args);
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
    let supersuper_altargty = Punctuated::<Type, token::Comma>::from_iter(
        altargty.iter()
        .map(|t| supersuperfy(&t))
    );
    let cm_ts = common_methods(&egenerics, &argnames, &altargty);
    let em_ts = expectation_methods(&vis, &argnames, &altargty);
    let eem_ts = expectations_methods(&vis, &egenerics);
    let gem_ts = generic_expectation_methods(&vis);
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
            #vis struct Expectation #egenerics_ig {
                common: Common #egenerics_tg,
                result: Option<#output>,
            }

            impl #egenerics_ig Expectation #egenerics_tg {
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

                #em_ts
            }

            impl #egenerics_ig Default for Expectation #egenerics_tg
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        result: None
                    }
                }
            }

            #eem_ts
            impl #egenerics_ig Expectations #egenerics_tg {
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
            impl #egenerics_ig
                ::mockall::AnyExpectations for Expectations #egenerics_tg
                    where #output: Send + Sync
            {}

            #gem_ts
            impl GenericExpectations {
                /// Simulating calling the real method.
                #vis fn call #egenerics_ig
                    (&self, #selfless_args ) -> &#output
                {
                    self.store.get(&::mockall::Key::new::<(#argty_tp)>())
                        .expect("No matching expectation found")
                        .downcast_ref::<Expectations #egenerics_tg>()
                        .unwrap()
                        .call(#argnames)
                }

                /// Create a new Expectation.
                #vis fn expect #eltgenerics_ig (&'lt mut self)
                    -> &'lt mut Expectation #egenerics_tg
                    #eltgenerics_wc
                    where #output: Send + Sync
                {
                    self.store.entry(::mockall::Key::new::<(#argty_tp)>())
                        .or_insert_with(||
                            Box::new(Expectations #egenerics_tbf ::new())
                        ).downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .expect()
                }
            }
        }
        )
    } else if ref_mut_expectation {
        let cm_ts = common_methods(&egenerics, &argnames, &altargty);
        let em_ts = expectation_methods(&vis, &argnames, &altargty);
        let eem_ts = expectations_methods(&vis, &egenerics);
        let gem_ts = generic_expectation_methods(&vis);
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
            #vis struct Expectation #egenerics_ig {
                common: Common #egenerics_tg,
                result: Option<#output>,
                rfunc: Option<Box<dyn FnMut(#argty) -> #output + Send + Sync>>,
            }

            impl #egenerics_ig Expectation #egenerics_tg {
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

                #em_ts
            }
            impl #egenerics_ig Default for Expectation #egenerics_tg
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        result: None,
                        rfunc: None
                    }
                }
            }
            #eem_ts
            impl #egenerics_ig Expectations #egenerics_tg {
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
            impl #egenerics_ig
                ::mockall::AnyExpectations for Expectations #egenerics_tg
                where #output: Send + Sync
            {}

            #gem_ts
            impl GenericExpectations {
                /// Simulating calling the real method.
                #vis fn call_mut #egenerics_ig
                    (&mut self, #selfless_args ) -> &mut #output
                {
                    self.store.get_mut(&::mockall::Key::new::<(#argty_tp)>())
                        .expect("No matching expectation found")
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .call_mut(#argnames)
                }

                /// Create a new Expectation.
                #vis fn expect #eltgenerics_ig (&'lt mut self)
                    -> &'lt mut Expectation #egenerics_tg
                    #eltgenerics_wc
                    where #output: Send + Sync
                {
                    self.store.entry(::mockall::Key::new::<(#argty_tp)>())
                        .or_insert_with(||
                            Box::new(Expectations #egenerics_tbf ::new())
                        ).downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .expect()
                }
            }
        }
        )
    } else if static_method {
        let refaltargty = TokenStream::from_iter(
            altargty.iter().map(|aaty| quote!(&#aaty,))
        );
        let pred_argnames = Punctuated::<Ident, Token![,]>::from_iter(
            (0..altargty.len())
            .map(|i| Ident::new(&format!("mockall_g{}", i), Span::call_site()))
        );
        let pred_arg_tys = (0..altargty.len())
            .map(|i| Ident::new(&format!("MockallG{}", i), Span::call_site()))
            .collect::<Vec<_>>();
        let pred_params = Punctuated::<TokenStream, Token![,]>::from_iter(
            pred_arg_tys.iter().zip(altargty.iter()).map(|(ident, ty)|
                quote!(#ident: ::mockall::Predicate<#ty> + Send + 'static)
            )
        );
        let pred_args = Punctuated::<TokenStream, Token![,]>::from_iter(
            pred_argnames.iter().zip(pred_arg_tys.into_iter())
            .map(|(pred_arg_name, ident)| quote!(#pred_arg_name: #ident))
        );
        let pred_generics = quote!(< #pred_params >);
        let ts = static_expectation(vis, &egenerics, &selfless_args, &argnames,
                                    &supersuper_argty, &supersuper_altargty,
                                    &matchexprs, &supersuper_output);
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
            #vis struct ExpectationGuard #eltgenerics_ig #eltgenerics_wc {
                guard: MutexGuard<'lt, Expectations #egenerics_tg>,
                i: usize
            }

            impl #eltgenerics_ig ExpectationGuard #eltgenerics_tg
                #eltgenerics_wc
            {
                /// Just like
                /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                #vis fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
                    -> &mut Expectation #egenerics_tg
                {
                    self.guard.0[self.i].in_sequence(seq)
                }

                /// Just like
                /// [`Expectation::never`](struct.Expectation.html#method.never)
                #vis fn never(&mut self) -> &mut Expectation #egenerics_tg {
                    self.guard.0[self.i].never()
                }

                // Should only be called from the mockall_derive generated code
                #[doc(hidden)]
                #vis fn new(mut guard: MutexGuard<'lt, Expectations #egenerics_tg>)
                    -> Self
                {
                    guard.expect(); // Drop the &Expectation
                    let i = guard.0.len() - 1;
                    ExpectationGuard{guard, i}
                }

                /// Just like [`Expectation::once`](struct.Expectation.html#method.once)
                #vis fn once(&mut self) -> &mut Expectation #egenerics_tg {
                    self.guard.0[self.i].once()
                }

                /// Just like
                /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                #vis fn returning<F>(&mut self, f: F)
                    -> &mut Expectation #egenerics_tg
                    where F: FnMut(#argty) -> #output + Send + 'static
                {
                    self.guard.0[self.i].returning(f)
                }

                /// Just like
                /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                #vis fn return_once<F>(&mut self, f: F)
                    -> &mut Expectation #egenerics_tg
                    where F: FnOnce(#argty) -> #output + Send + 'static
                {
                    self.guard.0[self.i].return_once(f)
                }

                /// Just like
                /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                #vis fn returning_st<F>(&mut self, f: F)
                    -> &mut Expectation #egenerics_tg
                    where F: FnMut(#argty) -> #output + 'static
                {
                    self.guard.0[self.i].returning_st(f)
                }

                /// Just like
                /// [`Expectation::times`](struct.Expectation.html#method.times)
                #vis fn times(&mut self, n: usize)
                    -> &mut Expectation #egenerics_tg {
                    self.guard.0[self.i].times(n)
                }

                /// Just like
                /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                #vis fn times_any(&mut self) -> &mut Expectation #egenerics_tg {
                    self.guard.0[self.i].times_any()
                }

                /// Just like
                /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                #vis fn times_range(&mut self, range: Range<usize>)
                    -> &mut Expectation #egenerics_tg
                {
                    self.guard.0[self.i].times_range(range)
                }

                /// Just like
                /// [`Expectation::with`](struct.Expectation.html#method.with)
                #vis fn with #pred_generics (&mut self, #pred_args)
                    -> &mut Expectation #egenerics_tg
                {
                    self.guard.0[self.i].with(#pred_argnames)
                }

                /// Just like
                /// [`Expectation::withf`](struct.Expectation.html#method.withf)
                #vis fn withf<F>(&mut self, f: F) -> &mut Expectation #egenerics_tg
                    where F: Fn(#refaltargty) -> bool + Send + 'static
                {
                    self.guard.0[self.i].withf(f)
                }
            }

            /// Like a [`ExpectationGuard`](struct.ExpectationGuard.html) but for
            /// generic methods.
            #vis struct GenericExpectationGuard #eltgenerics_ig #eltgenerics_wc{
                guard: MutexGuard<'lt, GenericExpectations>,
                i: usize,
                _phantom: PhantomData<(#fn_params)>,
            }

            impl #eltgenerics_ig GenericExpectationGuard #eltgenerics_tg
                #eltgenerics_wc
            {
                /// Just like
                /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                #vis fn in_sequence(&mut self, seq: &mut ::mockall::Sequence)
                    -> &mut Expectation #egenerics_tg
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .in_sequence(seq)
                }

                /// Just like
                /// [`Expectation::never`](struct.Expectation.html#method.never)
                #vis fn never(&mut self) -> &mut Expectation #egenerics_tg {
                        self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .never()
                }

                #[doc(hidden)]
                #vis fn new(mut guard: MutexGuard<'lt, GenericExpectations>)
                    -> Self
                {
                    let __ee: &mut Expectations #egenerics_tg =
                        guard.store.entry(::mockall::Key::new::<(#argty_tp)>())
                        .or_insert_with(||
                            Box::new(Expectations #egenerics_tbf ::new()))
                        .downcast_mut()
                        .unwrap();
                    __ee.expect();    // Drop the &Expectation
                    let i = __ee.0.len() - 1;
                    GenericExpectationGuard{guard, i, _phantom: PhantomData}
                }

                /// Just like
                /// [`Expectation::once`](struct.Expectation.html#method.once)
                #vis fn once(&mut self) -> &mut Expectation #egenerics_tg {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .once()
                }

                /// Just like
                /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                #vis fn returning<F>(&mut self, f: F) -> &mut Expectation #egenerics_tg
                    where F: FnMut(#argty) -> #output + Send + 'static
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .returning(f)
                }

                /// Just like
                /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                #vis fn return_once<F>(&mut self, f: F) -> &mut Expectation #egenerics_tg
                    where F: FnOnce(#argty) -> #output + Send + 'static
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .return_once(f)
                }

                /// Just like
                /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                #vis fn returning_st<F>(&mut self, f: F) -> &mut Expectation #egenerics_tg
                    where F: FnMut(#argty) -> #output + 'static
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .returning_st(f)
                }

                /// Just like
                /// [`Expectation::times`](struct.Expectation.html#method.times)
                #vis fn times(&mut self, n: usize) -> &mut Expectation #egenerics_tg {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .times(n)
                }

                /// Just like
                /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                #vis fn times_any(&mut self) -> &mut Expectation #egenerics_tg {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .times_any()
                }

                /// Just like
                /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                #vis fn times_range(&mut self, range: Range<usize>)
                    -> &mut Expectation #egenerics_tg
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .times_range(range)
                }

                /// Just like
                /// [`Expectation::with`](struct.Expectation.html#method.with)
                #vis fn with #pred_generics (&mut self, #pred_args)
                    -> &mut Expectation #egenerics_tg
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .with(#pred_argnames)
                }

                /// Just like
                /// [`Expectation::withf`](struct.Expectation.html#method.withf)
                #vis fn withf<F>(&mut self, f: F) -> &mut Expectation #egenerics_tg
                    where F: Fn(#refaltargty) -> bool + Send + 'static
                {
                    self.guard.store.get_mut(
                            &::mockall::Key::new::<(#argty_tp)>()
                        ).unwrap()
                        .downcast_mut::<Expectations #egenerics_tg>()
                        .unwrap()
                        .0[self.i]
                        .withf(f)
                }
            }
            }
        )
    } else {
        let ts = static_expectation(vis, &egenerics, &selfless_args, &argnames,
                                    &supersuper_argty, &supersuper_altargty,
                                    &matchexprs, &supersuper_output);
        quote!(
            #attrs
            pub mod #ident { #ts }
        )
    }
}
