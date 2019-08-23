// vim: tw=80

use super::*;
use quote::ToTokens;

/// Convert a special reference type like "&str" into a reference to its owned
/// type like "&String".
fn destrify(ty: &mut Type) {
    if let Type::Reference(ref mut tr) = ty {
        if let Some(lt) = &tr.lifetime {
            if lt.ident == "static" {
                // For methods that return 'static references, the user can
                // usually actually supply one, unlike nonstatic references.
                // destrify is unneeded and harmful in such cases.
                return;
            }
        }

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

/// Stuff that's common between all Expectation types
struct Common<'a> {
    /// Names of the method arguments
    argnames: Vec<Pat>,
    /// Types of the method arguments
    argty: Vec<Type>,
    attrs: &'a TokenStream,
    /// The expectation's generic types as a list of types
    fn_params: Punctuated<Ident, Token![,]>,
    /// Generics of the parent struct
    struct_generics: Option<&'a Generics>,
    /// Generics of the method
    meth_generics: &'a Generics,
    /// Generics of the Expectation object
    egenerics: Generics,
    /// Is this for a static method or free function?
    is_static: bool,
    /// Expressions that create the predicate arguments from the call arguments
    predexprs: Vec<TokenStream>,
    /// Types used for Predicates.  Will be almost the same as args, but every
    /// type will be a non-reference type.
    predty: Vec<Type>,
    /// name of the original method
    meth_ident: &'a Ident,
    /// name of the expectation's private module
    mod_ident: &'a Ident,
    /// Output type of the Method, supersuperfied.
    output: Type,
    /// Identifier of the parent structure, if any
    parent_ident: Option<&'a Ident>,
    /// Visibility of the expectation
    /// TODO: supersuperfy it here rather than in the caller
    vis: Visibility
}

impl<'a> Common<'a> {
    /// Methods of the Expectation structs that are common for static
    /// expectations, ref expectations, and ref mut expectations
    fn expectation_methods(&self, with_generics: &TokenStream,
        with_args: &TokenStream) -> TokenStream
    {
        let v = &self.vis;
        let argnames = &self.argnames;
        let predty = &self.predty;
        quote!(
            /// Add this expectation to a
            /// [`Sequence`](../../../mockall/struct.Sequence.html).
            #v fn in_sequence(&mut self, __mockall_seq: &mut ::mockall::Sequence)
                -> &mut Self
            {
                self.common.in_sequence(__mockall_seq);
                self
            }

            fn is_done(&self) -> bool {
                self.common.is_done()
            }

            /// Validate this expectation's matcher.
            fn matches(&self, #(#argnames: &#predty, )*) -> bool {
                self.common.matches(#(#argnames, )*)
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

            /// Restrict the number of times that that this method may be called.
            ///
            /// The argument may be:
            /// * A fixed number: `.times(4)`
            /// * Various types of range:
            ///   - `.times(5..10)`
            ///   - `.times(..10)`
            ///   - `.times(5..)`
            ///   - `.times(5..=10)`
            ///   - `.times(..=10)`
            /// * The wildcard: `.times(..)`
            #v fn times<MockallR>(&mut self, __mockall_r: MockallR) -> &mut Self
                where MockallR: Into<::mockall::TimesRange>
            {
                self.common.times(__mockall_r);
                self
            }

            /// Allow this expectation to be called any number of times
            ///
            /// This behavior is the default, but the method is provided in case the
            /// default behavior changes.
            #[deprecated(since = "0.3.0", note = "Use times instead")]
            #v fn times_any(&mut self) -> &mut Self {
                self.common.times(..);
                self
            }

            /// Allow this expectation to be called any number of times within a
            /// given range
            #[deprecated(since = "0.3.0", note = "Use times instead")]
            #v fn times_range(&mut self, __mockall_range: Range<usize>)
                -> &mut Self
            {
                self.common.times(__mockall_range);
                self
            }

            /// Set matching crieteria for this Expectation.
            ///
            /// The matching predicate can be anything implemening the
            /// [`Predicate`](../../../mockall/trait.Predicate.html) trait.  Only
            /// one matcher can be set per `Expectation` at a time.
            #v fn with<#with_generics>(&mut self, #with_args) -> &mut Self
            {
                self.common.with(#(#argnames, )*);
                self
            }

            /// Set a matching function for this Expectation.
            ///
            /// This is equivalent to calling [`with`](#method.with) with a function
            /// argument, like `with(predicate::function(f))`.
            #v fn withf<MockallF>(&mut self, __mockall_f: MockallF) -> &mut Self
                where MockallF: Fn(#(&#predty, )*) -> bool + Send + 'static
            {
                self.common.withf(__mockall_f);
                self
            }
        )
    }

    /// Common methods of the Expectations structs
    fn expectations_methods(&self) -> TokenStream {
        let (ig, tg, wc) = self.egenerics.split_for_impl();
        let v = &self.vis;
        quote!(
            /// A collection of [`Expectation`](struct.Expectations.html)
            /// objects.  Users will rarely if ever use this struct directly.
            #[doc(hidden)]
            #v struct Expectations #ig ( Vec<Expectation #tg>) #wc;

            impl #ig Expectations #tg #wc {
                /// Verify that all current expectations are satisfied and clear
                /// them.
                #v fn checkpoint(&mut self) -> std::vec::Drain<Expectation #tg>
                {
                    self.0.drain(..)
                }

                /// Create a new expectation for this method.
                #v fn expect(&mut self) -> &mut Expectation #tg
                {
                    self.0.push(Expectation::default());
                    let __mockall_l = self.0.len();
                    &mut self.0[__mockall_l - 1]
                }

                #v fn new() -> Self {
                    Self::default()
                }
            }
            impl #ig Default for Expectations #tg #wc
            {
                fn default() -> Self {
                    Expectations(Vec::new())
                }
            }
        )
    }

    fn generic_expectations_methods(&self) -> TokenStream
    {
        let v = &self.vis;
        if !self.is_generic() {
            return TokenStream::new();
        }
        quote!(
            /// A collection of [`Expectation`](struct.Expectations.html)
            /// objects for a generic method.  Users will rarely if ever use
            /// this struct directly.
            #[doc(hidden)]
            #[derive(Default)]
            #v struct GenericExpectations{
                store: std::collections::hash_map::HashMap<::mockall::Key,
                               Box<dyn ::mockall::AnyExpectations>>
            }
            impl GenericExpectations {
                /// Verify that all current expectations are satisfied and clear
                /// them.  This applies to all sets of generic parameters!
                #v fn checkpoint(&mut self) ->
                    std::collections::hash_map::Drain<::mockall::Key,
                               Box<dyn ::mockall::AnyExpectations>>
                {
                    self.store.drain()
                }

                #v fn new() -> Self {
                    Self::default()
                }
            }
        )
    }

    fn is_generic(&self) -> bool {
        !self.egenerics.params.is_empty() ||
            self.egenerics.where_clause.is_some()
    }
}

pub(crate) enum Expectation<'a> {
    Ref(RefExpectation<'a>),
    RefMut(RefMutExpectation<'a>),
    Static(StaticExpectation<'a>)
}

macro_rules! dispatch {
    ($self:ident, $func:ident $(,$args:ident)*) => {
        match $self {
            Expectation::Ref(e) => e.$func($($args, )*),
            Expectation::RefMut(e) => e.$func($($args, )*),
            Expectation::Static(e) => e.$func($($args, )*),
        }
    }
}

impl<'a> Expectation<'a> {
    fn common(&self) -> &Common {dispatch!(self, common)}
    fn expectation(&self, em_ts: TokenStream) -> TokenStream {
        dispatch!(self, expectation, em_ts)
    }
    fn expectations_methods(&self) -> TokenStream {
        dispatch!(self, expectations_methods)
    }
    fn extra_uses(&self) -> TokenStream {dispatch!(self, extra_uses)}
    fn generic_expectations_methods(&self) -> TokenStream {
        if !self.common().is_generic() {
            return TokenStream::new();
        }
        dispatch!(self, generic_expectations_methods)
    }

    pub(crate) fn gen(&self) -> TokenStream {
        let argnames = &self.common().argnames;
        let attrs = &self.common().attrs;
        let ident = &self.common().mod_ident;
        let ident_str = if let Some(pi) = self.common().parent_ident {
            format!("{}::{}", pi, self.common().meth_ident)
        } else {
            format!("{}", self.common().meth_ident)
        };
        let extra_uses = self.extra_uses();
        let fn_params = &self.common().fn_params;
        let predty = &self.common().predty;
        let rfunc_ts = self.rfunc();
        let (ig, tg, wc) = self.common().egenerics.split_for_impl();
        let preds = TokenStream::from_iter(
            self.common().predty.iter().map(|t|
                quote!(Box<dyn ::mockall::Predicate<#t> + Send>,)
            )
        );
        let pred_matches = TokenStream::from_iter(
            argnames.iter().enumerate()
            .map(|(i, argname)| {
                let idx = syn::Index::from(i);
                quote!(__mockall_pred.#idx.eval(#argname),)
            })
        );
        let pred_verify = TokenStream::from_iter(
            argnames.iter().enumerate()
            .map(|(i, argname)| {
                let idx = Index::from(i);
                quote!(
                    if let Some(__mockall_c) =
                        __mockall_pred.#idx.find_case(false, #argname)
                    {
                        panic!("Expectation didn't match arguments:\n{}",
                               __mockall_c.tree());
                    }
                )
            })
        );
        let refpredty = TokenStream::from_iter(
            self.common().predty.iter().map(|mt| quote!(&#mt,))
        );
        let with_generics_idents = (0..self.common().predty.len())
            .map(|i| format_ident!("MockallMatcher{}", i))
            .collect::<Vec<_>>();
        let with_generics = TokenStream::from_iter(
            with_generics_idents.iter().zip(self.common().predty.iter())
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
        let braces = argnames.iter().map(|_| "{}").collect::<Vec<_>>();
        let indices = (0..argnames.len())
            .map(|i| {
                let idx = syn::Index::from(i);
                idx
            }).collect::<Vec<_>>();
        let matcher_ts = quote!(
            enum Matcher #ig #wc {
                Func(Box<dyn Fn(#refpredty) -> bool + Send>),
                Pred(Box<(#preds)>),
                // Prevent "unused type parameter" errors
                // Surprisingly, PhantomData<Fn(generics)> is Send even if
                // generics are not, unlike PhantomData<generics>
                _Phantom(Box<dyn Fn(#fn_params) -> () + Send>)
            }

            impl #ig Matcher #tg #wc {
                fn matches(&self, #( #argnames: &#predty, )*) -> bool {
                    match self {
                        Matcher::Func(__mockall_f) =>
                            __mockall_f(#(#argnames, )*),
                        Matcher::Pred(__mockall_pred) =>
                            [#pred_matches]
                            .into_iter()
                            .all(|__mockall_x| *__mockall_x),
                        _ => unreachable!()
                    }
                }

                fn verify(&self, #( #argnames: &#predty, )* ) {
                    match self {
                        Matcher::Func(__mockall_f) =>
                            assert!(__mockall_f(#(#argnames, )*),
                            "Expectation didn't match arguments"),
                        Matcher::Pred(__mockall_pred) => { #pred_verify },
                        _ => unreachable!()
                    }
                }
            }

            impl #ig Default for Matcher #tg #wc {
                #[allow(unused_variables)]
                fn default() -> Self {
                    Matcher::Func(Box::new(|#(#argnames, )*| true))
                }
            }

            impl #ig ::std::fmt::Display for Matcher #tg #wc {
                fn fmt(&self, __mockall_fmt: &mut ::std::fmt::Formatter<'_>)
                    -> ::std::fmt::Result
                {
                    match self {
                        Matcher::Func(_) => write!(__mockall_fmt, "TODO"),
                        Matcher::Pred(__mockall_p) => {
                            write!(__mockall_fmt, concat!(#(#braces,)*),
                                #(__mockall_p.#indices,)*)
                        }
                        _ => unreachable!(),
                    }
                }
            }

            /// Holds the stuff that is independent of the output type
            struct Common #ig #wc {
                matcher: Mutex<Matcher #tg>,
                seq_handle: Option<::mockall::SeqHandle>,
                times: ::mockall::Times
            }

            impl #ig std::default::Default for Common #tg #wc
            {
                fn default() -> Self {
                    Common {
                        matcher: Mutex::new(Matcher::default()),
                        seq_handle: None,
                        times: ::mockall::Times::new(#ident_str)
                    }
                }
            }

            impl #ig Common #tg #wc {
                fn call(&self, #( #argnames: &#predty, )* ) {
                    self.matcher.lock().unwrap().verify(#(#argnames, )*);
                    self.times.call();
                    self.verify_sequence();
                    if self.times.is_satisfied() {
                        self.satisfy_sequence()
                    }
                }

                fn in_sequence(&mut self, __mockall_seq: &mut ::mockall::Sequence)
                    -> &mut Self
                {
                    assert!(self.times.is_exact(),
                        "Only Expectations with an exact call count have sequences");
                    self.seq_handle = Some(__mockall_seq.next_handle());
                    self
                }

                fn is_done(&self) -> bool {
                    self.times.is_done()
                }

                fn matches(&self, #( #argnames: &#predty, )*) -> bool {
                    self.matcher.lock().unwrap().matches(#(#argnames, )*)
                }

                /// Forbid this expectation from ever being called.
                fn never(&mut self) {
                    self.times.never();
                }

                fn satisfy_sequence(&self) {
                    if let Some(__mockall_handle) = &self.seq_handle {
                        __mockall_handle.satisfy()
                    }
                }

                /// Expect this expectation to be called any number of times
                /// contained with the given range.
                fn times<MockallR>(&mut self, __mockall_r: MockallR)
                    where MockallR: Into<::mockall::TimesRange>
                {
                    self.times.times(__mockall_r)
                }

                fn with<#with_generics>(&mut self, #with_args)
                {
                    let mut __mockall_guard = self.matcher.lock().unwrap();
                    mem::replace(__mockall_guard.deref_mut(),
                        Matcher::Pred(Box::new((#boxed_withargs))));
                }

                fn withf<MockallF>(&mut self, __mockall_f: MockallF)
                    where MockallF: Fn(#refpredty) -> bool + Send + 'static
                {
                    let mut __mockall_guard = self.matcher.lock().unwrap();
                    mem::replace(__mockall_guard.deref_mut(),
                                 Matcher::Func(Box::new(__mockall_f)));
                }

                fn verify_sequence(&self) {
                    if let Some(__mockall_handle) = &self.seq_handle {
                        __mockall_handle.verify()
                    }
                }
            }
        );
        let em_ts = self.common().expectation_methods(&with_generics,
                                                      &with_args);
        let expectation_ts = self.expectation(em_ts);
        let eem_ts1 = self.common().expectations_methods();
        let eem_ts2 = self.expectations_methods();
        let gem_ts1 = self.common().generic_expectations_methods();
        let gem_ts2 = self.generic_expectations_methods();
        let sm_ts = self.static_method_methods(&with_generics, &with_args);
        quote!(
            #attrs
            pub mod #ident {
                #extra_uses
                use super::*;   // Import types from the calling environment
                use ::mockall::CaseTreeExt;
                use ::std::{
                    mem,
                    ops::{DerefMut, Range},
                    sync::Mutex
                };
                #rfunc_ts
                #matcher_ts
                #expectation_ts
                #eem_ts1
                #eem_ts2
                #gem_ts1
                #gem_ts2
                #sm_ts
            }
        )
    }

    /// # Arguments
    /// * `attrs`           - Any additional attributes on this method, like
    ///                       `#[cfg()]`
    /// * `args`            - Arguments for the mock method, which may be
    ///                       slightly different than on the original method.
    /// * `struct_generics` - Generics of the parent struct, if any.
    /// * `meth_generics`   - Generics of the method being mocked
    /// * `mod_ident`       - Name of the expectaton's private module
    /// * `meth_ident`      - Name of the original method
    /// * `parent_ident`    - Name of the parent struct, if any.
    /// * `return_type`     - Return type of the mock method
    /// * `vis`             - Visibility of the expectation, *already supersuperfied*.
    pub(crate) fn new(
        attrs: &'a TokenStream,
        args: &Punctuated<FnArg, Token![,]>,
        struct_generics: Option<&'a Generics>,
        meth_generics: &'a Generics,
        meth_ident: &'a Ident,
        mod_ident: &'a Ident,
        parent_ident: Option<&'a Ident>,
        return_type: &ReturnType,
        vis: &Visibility) -> Self
    {
        // Too bad Iterator::unzip only works on 2-tuples
        let mut argnames = Vec::new();
        let mut argty = Vec::new();
        let mut is_static = true;
        let mut predexprs = Vec::new();
        let mut predty = Vec::new();
        for fa in args.iter() {
            if let FnArg::Typed(pt) = fa {
                let argname = (*pt.pat).clone();
                let aty = supersuperfy(&pt.ty);
                if let Type::Reference(ref tr) = aty {
                    predexprs.push(quote!(#argname));
                    predty.push((*tr.elem).clone());
                } else {
                    predexprs.push(quote!(&#argname));
                    predty.push(aty.clone());
                };
                argnames.push(argname);
                argty.push(aty);
            } else {
                is_static = false;
                ()    // Strip out the "&self" argument
            }
        }
        let generics = if let Some(g) = struct_generics {
            merge_generics(g, meth_generics)
        } else {
            meth_generics.clone()
        };
        let mut egenerics = generics.clone();
        for p in egenerics.params.iter_mut() {
            if let GenericParam::Type(tp) = p {
                let static_bound = Lifetime::new("'static", Span::call_site());
                tp.bounds.push(TypeParamBound::Lifetime(static_bound));
            }
        }
        let fn_params = Punctuated::<Ident, Token![,]>::from_iter(
            egenerics.type_params().map(|tp| tp.ident.clone())
        );
        let mut ref_expectation = false;
        let mut ref_mut_expectation = false;
        let output = supersuperfy(&match return_type {
            ReturnType::Default => Type::Tuple(TypeTuple {
                paren_token: token::Paren::default(),
                elems: Punctuated::new()
            }),
            ReturnType::Type(_, ref ty) => {
                let mut rt: Type = (**ty).clone();
                destrify(&mut rt);
                if let Some(i) = parent_ident {
                    crate::deselfify(&mut rt, i, &struct_generics.unwrap());
                }
                if let Type::Reference(ref tr) = rt {
                    if tr.lifetime.as_ref()
                        .map_or(false, |lt| lt.ident == "static")
                    {
                        // Just a static expectation
                        rt
                    } else {
                        if tr.mutability.is_none() {
                            ref_expectation = true;
                        } else {
                            ref_mut_expectation = true;
                        }
                        (*tr.elem).clone()
                    }
                } else {
                    rt
                }
            }
        });

        let common = Common {
            argnames,
            argty,
            attrs,
            fn_params,
            struct_generics,
            meth_generics,
            egenerics,
            is_static,
            predexprs,
            predty,
            meth_ident,
            mod_ident,
            output,
            parent_ident,
            vis: vis.clone()
        };
        if ref_mut_expectation {
            Expectation::RefMut(RefMutExpectation::new(common))
        } else if ref_expectation {
            Expectation::Ref(RefExpectation::new(common))
        } else {
            Expectation::Static(StaticExpectation::new(common))
        }
    }

    fn rfunc(&self) -> TokenStream { dispatch!(self, rfunc) }

    fn static_method_methods(&self, with_generics: &TokenStream,
        with_args: &TokenStream) -> TokenStream
    {
        dispatch!(self, static_method_methods, with_generics, with_args)
    }
}

impl<'a> ToTokens for Expectation<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.gen().to_tokens(tokens)
    }
}

/// For methods that return a 'static value
pub(crate) struct StaticExpectation<'a> {
    common: Common<'a>,
}

impl<'a> StaticExpectation<'a> {
    fn new(common: Common<'a>) -> Self {
        StaticExpectation {
            common,
        }
    }

    fn common(&self) -> &Common {&self.common}

    fn expectation(&self, em_ts: TokenStream) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predexprs = &self.common.predexprs;
        let v = &self.common.vis;
        quote!(
            /// Expectation type for methods that return a `'static` type.
            /// This is the type returned by the `expect_*` methods.
            #v struct Expectation #ig #wc {
                common: Common #tg,
                rfunc: Mutex<Rfunc #tg>,
            }

            impl #ig Expectation #tg #wc {
                /// Call this [`Expectation`] as if it were the real method.
                #[doc(hidden)]
                #v fn call(&self, #(#argnames: #argty, )* ) -> #output
                {
                    self.common.call(#(#predexprs, )*);
                    self.rfunc.lock().unwrap().call_mut(#(#argnames, )*)
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
                #v fn return_const<MockallOutput>(&mut self, __mockall_c: MockallOutput)
                    -> &mut Self
                    where MockallOutput: Clone + Into<#output> + Send + 'static
                {
                    self.returning(move |#(#argnames, )*| __mockall_c.clone().into())
                }

                /// Supply an `FnOnce` closure that will provide the return value
                /// for this Expectation.  This is useful for return types that
                /// aren't `Clone`.  It will be an error to call this method
                /// multiple times.
                #v fn return_once<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Self
                    where MockallF: FnOnce(#(#argty, )*) -> #output + Send + 'static
                {
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        mem::replace(__mockall_guard.deref_mut(),
                            Rfunc::Once(Box::new(__mockall_f)));
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
                #v fn return_once_st<MockallF>(&mut self, __mockall_f:
                                                  MockallF) -> &mut Self
                    where MockallF: FnOnce(#(#argty, )*) -> #output + 'static
                {
                    let __mockall_fragile = ::mockall::Fragile::new(__mockall_f);
                    let __mockall_fonce = Box::new(move |#(#argnames: #argty, )*| {
                        (__mockall_fragile.into_inner())(#(#argnames, )*)
                    });
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        mem::replace(__mockall_guard.deref_mut(), Rfunc::Once(__mockall_fonce));
                    }
                    self
                }

                /// Supply a closure that will provide the return value for this
                /// `Expectation`.  The method's arguments are passed to the closure
                /// by value.
                #v fn returning<MockallF>(&mut self, __mockall_f: MockallF) -> &mut Self
                    where MockallF: FnMut(#(#argty, )*) -> #output + Send + 'static
                {
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        mem::replace(__mockall_guard.deref_mut(),
                            Rfunc::Mut(Box::new(__mockall_f)));
                    }
                    self
                }

                /// Single-threaded version of [`returning`](#method.returning).
                /// Can be used when the argument or return type isn't `Send`.
                ///
                /// It is a runtime error to call the mock method from a different
                /// thread than the one that originally called this method.
                #v fn returning_st<MockallF>(&mut self, __mockall_f: MockallF) -> &mut Self
                    where MockallF: FnMut(#(#argty, )*) -> #output + 'static
                {
                    let mut __mockall_fragile = ::mockall::Fragile::new(__mockall_f);
                    let __mockall_fmut = move |#(#argnames: #argty, )*| {
                        (__mockall_fragile.get_mut())(#(#argnames, )*)
                    };
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        mem::replace(__mockall_guard.deref_mut(),
                            Rfunc::Mut(Box::new(__mockall_fmut)));
                    }
                    self
                }

                #em_ts
            }
            impl #ig Default for Expectation #tg #wc
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        rfunc: Mutex::new(Rfunc::default())
                    }
                }
            }
        )
    }

    fn expectations_methods(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predexprs = &self.common.predexprs;
        let v = &self.common.vis;
        quote!(
            impl #ig Expectations #tg #wc {
                /// Simulate calling the real method.  Every current expectation
                /// will be checked in FIFO order and the first one with
                /// matching arguments will be used.
                #v fn call(&self, #(#argnames: #argty, )* ) -> Option<#output>
                {
                    self.0.iter()
                        .find(|__mockall_e|
                              __mockall_e.matches(#(#predexprs, )*) &&
                              (!__mockall_e.is_done() || self.0.len() == 1))
                        .map(move |__mockall_e|
                             __mockall_e.call(#(#argnames, )*)
                        )
                }

            }
            impl #ig ::mockall::AnyExpectations for Expectations #tg #wc {}
        )
    }

    fn extra_uses(&self) -> TokenStream {
        if self.common.is_static {
            quote!(use ::std::sync::MutexGuard;)
        } else {
            TokenStream::new()
        }
    }

    fn generic_expectations_methods(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let tbf = tg.as_turbofish();
        let v = &self.common.vis;
        quote!(
            impl GenericExpectations {
                /// Simulating calling the real method.
                #v fn call #ig (&self, #(#argnames: #argty, )* )
                    -> Option<#output> #wc
                {
                    self.store.get(&::mockall::Key::new::<(#(#argty, )*)>())
                        .map(|__mockall_e| {
                            __mockall_e.downcast_ref::<Expectations #tg>()
                            .unwrap()
                            .call(#(#argnames, )*)
                        // TODO: use Option::flatten once it stabilizes
                        // https://github.com/rust-lang/rust/issues/60258
                        }).and_then(std::convert::identity)
                }

                /// Create a new Expectation.
                #v fn expect #ig (&mut self) -> &mut Expectation #tg #wc
                {
                    self.store.entry(::mockall::Key::new::<(#(#argty, )*)>())
                        .or_insert_with(|| Box::new(Expectations #tbf::new()))
                        .downcast_mut::<Expectations #tg>()
                        .unwrap()
                        .expect()
                }
            }
        )
    }

    fn rfunc(&self) -> TokenStream {
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let fn_params = &self.common.fn_params;
        let output = &self.common.output;
        quote!(
            enum Rfunc #ig #wc {
                Default,
                // Indicates that a `return_once` expectation has already
                // returned
                Expired,
                Mut(Box<dyn FnMut(#(#argty, )*) -> #output + Send>),
                Once(Box<dyn FnOnce(#(#argty, )*) -> #output + Send>),
                // Prevent "unused type parameter" errors Surprisingly,
                // PhantomData<Fn(generics)> is Send even if generics are not,
                // unlike PhantomData<generics>
                _Phantom(Box<dyn Fn(#fn_params) -> () + Send>)
            }

            impl #ig  Rfunc #tg #wc {
                fn call_mut(&mut self, #( #argnames: #argty, )* ) -> #output {
                    match self {
                        Rfunc::Default => {
                            use ::mockall::ReturnDefault;
                            ::mockall::DefaultReturner::<#output>
                                ::return_default()
                        },
                        Rfunc::Expired => {
                            panic!("Called a method twice that was expected only once")
                        },
                        Rfunc::Mut(__mockall_f) => {
                            __mockall_f( #(#argnames, )* )
                        },
                        Rfunc::Once(_) => {
                            if let Rfunc::Once(mut __mockall_f) =
                                mem::replace(self, Rfunc::Expired) {
                                __mockall_f( #(#argnames, )* )
                            } else {
                                unreachable!()
                            }
                        },
                        Rfunc::_Phantom(_) => unreachable!()
                    }
                }
            }

            impl #ig std::default::Default for Rfunc #tg #wc
            {
                fn default() -> Self {
                    Rfunc::Default
                }
            }
        )
    }

    fn static_method_methods(&self, with_generics: &TokenStream,
        with_args: &TokenStream) -> TokenStream
    {
        if !self.common.is_static {
            return TokenStream::new()
        }

        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let fn_params = &self.common.fn_params;
        let (_ig, tg, _wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predty = &self.common.predty;
        let tbf = tg.as_turbofish();
        let v = &self.common.vis;

        let gd = Generics::default();
        let (s_ig, s_tg, s_wc) = self.common.struct_generics.unwrap_or(&gd)
            .split_for_impl();

        // Add a lifetime parameter, needed by MutexGuard
        let mut meth_generics = self.common.meth_generics.clone();
        let ltdef = LifetimeDef::new(
            Lifetime::new("'__mockall_lt", Span::call_site())
        );
        meth_generics.params.push(GenericParam::Lifetime(ltdef));
        let (meth_ig, _meth_tg, meth_wc) = meth_generics.split_for_impl();

        let mut e_generics = self.common.egenerics.clone();
        let ltdef = LifetimeDef::new(
            Lifetime::new("'__mockall_lt", Span::call_site())
        );
        e_generics.params.push(GenericParam::Lifetime(ltdef));
        let (e_ig, e_tg, e_wc) = e_generics.split_for_impl();

        let ctx_fn_params = match self.common.struct_generics {
            None => Punctuated::new(),
            Some(g) => Punctuated::<Ident, Token![,]>::from_iter(
                g.type_params().map(|tp| tp.ident.clone())
            )
        };

        let context_ts = quote!(
            #[must_use = "Context only serves to create expectations" ]
            #v struct Context #s_ig #s_wc {
                // Prevent "unused type parameter" errors
                // Surprisingly, PhantomData<Fn(generics)> is Send even if
                // generics are not, unlike PhantomData<generics>
                _phantom: ::std::marker::PhantomData<
                    Box<dyn Fn(#ctx_fn_params) -> () + Send>
                >
            }
            impl #s_ig Context #s_tg #s_wc {
                #v fn checkpoint(&self) {
                    Self::do_checkpoint()
                }
                #[doc(hidden)]
                #v fn do_checkpoint() {
                    let __mockall_timeses = EXPECTATIONS
                        .lock()
                        .unwrap()
                        .checkpoint()
                        .collect::<Vec<_>>();
                }

                #[cfg_attr(not(feature = "nightly"), must_use =
                    "Must set return value when not using the \"nightly\" feature")
                ]
                #v fn expect #meth_ig ( &self,) -> ExpectationGuard #e_tg
                    #meth_wc
                {
                    ExpectationGuard::new(EXPECTATIONS.lock().unwrap())
                }
            }
            impl #s_ig Default for Context #s_tg #s_wc {
                fn default() -> Self {
                    Context {_phantom: std::marker::PhantomData}
                }
            }
            impl #s_ig Drop for Context #s_tg #s_wc {
                fn drop(&mut self) {
                    if !std::thread::panicking() {
                        Self::do_checkpoint()
                    }
                }
            }
        );
        if !self.common.is_generic() {
            quote!(
                ::mockall::lazy_static! {
                    #v static ref EXPECTATIONS:
                        ::std::sync::Mutex<Expectations> =
                        ::std::sync::Mutex::new(Expectations::new());
                }
                /// Like an [`&Expectation`](struct.Expectation.html) but
                /// protected by a Mutex guard.  Useful for mocking static
                /// methods.  Forwards accesses to an `Expectation` object.
                // We must return the MutexGuard to the caller so he can
                // configure the expectation.  But we can't bundle both the
                // guard and the &Expectation into the same structure; the
                // borrow checker won't let us.  Instead we'll record the
                // expectation's position within the Expectations vector so we
                // can proxy its methods.
                //
                // ExpectationGuard is only defined for expectations that return
                // 'static return types.
                #v struct ExpectationGuard #e_ig #e_wc {
                    guard: MutexGuard<'__mockall_lt, Expectations #tg>,
                    i: usize
                }

                impl #e_ig ExpectationGuard #e_tg #e_wc
                {
                    /// Just like
                    /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                    #v fn in_sequence(&mut self,
                        __mockall_seq: &mut ::mockall::Sequence)
                        -> &mut Expectation #tg
                    {
                        self.guard.0[self.i].in_sequence(__mockall_seq)
                    }

                    /// Just like
                    /// [`Expectation::never`](struct.Expectation.html#method.never)
                    #v fn never(&mut self) -> &mut Expectation #tg {
                        self.guard.0[self.i].never()
                    }

                    // Should only be called from the mockall_derive generated
                    // code
                    #[doc(hidden)]
                    #v fn new(mut __mockall_guard: MutexGuard<'__mockall_lt, Expectations #tg>)
                        -> Self
                    {
                        __mockall_guard.expect(); // Drop the &Expectation
                        let __mockall_i = __mockall_guard.0.len() - 1;
                        ExpectationGuard{guard: __mockall_guard, i: __mockall_i}
                    }

                    /// Just like [`Expectation::once`](struct.Expectation.html#method.once)
                    #v fn once(&mut self) -> &mut Expectation #tg {
                        self.guard.0[self.i].once()
                    }

                    /// Just like
                    /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                    #v fn returning<MockallF>(&mut self, __mockall_f: MockallF)
                        -> &mut Expectation #tg
                        where MockallF: FnMut(#(#argty, )*)
                            -> #output + Send + 'static
                    {
                        self.guard.0[self.i].returning(__mockall_f)
                    }

                    /// Just like
                    /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                    #v fn return_once<MockallF>(&mut self, __mockall_f: MockallF)
                        -> &mut Expectation #tg
                        where MockallF: FnOnce(#(#argty, )*) -> #output + Send + 'static
                    {
                        self.guard.0[self.i].return_once(__mockall_f)
                    }

                    /// Just like
                    /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                    #v fn returning_st<MockallF>(&mut self, __mockall_f: MockallF)
                        -> &mut Expectation #tg
                        where MockallF: FnMut(#(#argty, )*) -> #output + 'static
                    {
                        self.guard.0[self.i].returning_st(__mockall_f)
                    }

                    /// Just like
                    /// [`Expectation::times`](struct.Expectation.html#method.times)
                    #v fn times<MockallR>(&mut self, __mockall_r: MockallR)
                        -> &mut Expectation #tg
                        where MockallR: Into<::mockall::TimesRange>
                    {
                        self.guard.0[self.i].times(__mockall_r)
                    }

                    /// Just like
                    /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                    #[deprecated(since = "0.3.0", note = "Use times instead")]
                    #v fn times_any(&mut self) -> &mut Expectation #tg {
                        self.guard.0[self.i].times(..)
                    }

                    /// Just like
                    /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                    #[deprecated(since = "0.3.0", note = "Use times instead")]
                    #v fn times_range(&mut self, __mockall_range: Range<usize>)
                        -> &mut Expectation #tg
                    {
                        self.guard.0[self.i].times(__mockall_range)
                    }

                    /// Just like
                    /// [`Expectation::with`](struct.Expectation.html#method.with)
                    #v fn with<#with_generics> (&mut self, #with_args)
                        -> &mut Expectation #tg
                    {
                        self.guard.0[self.i].with(#(#argnames, )*)
                    }

                    /// Just like
                    /// [`Expectation::withf`](struct.Expectation.html#method.withf)
                    #v fn withf<MockallF>(&mut self, __mockall_f: MockallF)
                        -> &mut Expectation #tg
                        where MockallF: Fn(#(&#predty, )*) -> bool + Send + 'static
                    {
                        self.guard.0[self.i].withf(__mockall_f)
                    }
                }
                #context_ts
            )
        } else {
            quote!(
                ::mockall::lazy_static! {
                    #v static ref EXPECTATIONS:
                        ::std::sync::Mutex<GenericExpectations> =
                        ::std::sync::Mutex::new(GenericExpectations::new());
                }
                /// Like an
                /// [`&GenericExpectation`](struct.GenericExpectation.html) but
                /// protected by a Mutex guard.  Useful for mocking static
                /// methods.  Forwards accesses to an `Expectation` object.
                #v struct ExpectationGuard #e_ig #e_wc{
                    guard: MutexGuard<'__mockall_lt, GenericExpectations>,
                    i: usize,
                    _phantom: ::std::marker::PhantomData<(#fn_params)>,
                }

                impl #e_ig ExpectationGuard #e_tg #e_wc
                {
                    /// Just like
                    /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
                    #v fn in_sequence(&mut self,
                        __mockall_seq: &mut ::mockall::Sequence)
                        -> &mut Expectation #tg
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .in_sequence(__mockall_seq)
                    }

                    /// Just like
                    /// [`Expectation::never`](struct.Expectation.html#method.never)
                    #v fn never(&mut self) -> &mut Expectation #tg {
                            self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .never()
                    }

                    #[doc(hidden)]
                    #v fn new(mut guard: MutexGuard<'__mockall_lt, GenericExpectations>)
                        -> Self
                    {
                        let __mockall_ee: &mut Expectations #tg =
                            guard.store.entry(
                                ::mockall::Key::new::<(#(#argty, )*)>()
                            ).or_insert_with(||
                                Box::new(Expectations #tbf ::new()))
                            .downcast_mut()
                            .unwrap();
                        __mockall_ee.expect();    // Drop the &Expectation
                        let __mockall_i = __mockall_ee.0.len() - 1;
                        ExpectationGuard{guard, i: __mockall_i,
                            _phantom: ::std::marker::PhantomData}
                    }

                    /// Just like
                    /// [`Expectation::once`](struct.Expectation.html#method.once)
                    #v fn once(&mut self) -> &mut Expectation #tg {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .once()
                    }

                    /// Just like
                    /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                    #v fn returning<MockallF>(&mut self, __mockall_f: MockallF)
                        -> &mut Expectation #tg
                        where MockallF: FnMut(#(#argty, )*)
                            -> #output + Send + 'static
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .returning(__mockall_f)
                    }

                    /// Just like
                    /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                    #v fn return_once<MockallF>(&mut self,
                        __mockall_f: MockallF) -> &mut Expectation #tg
                        where MockallF: FnOnce(#(#argty, )*)
                            -> #output + Send + 'static
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .return_once(__mockall_f)
                    }

                    /// Just like
                    /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                    #v fn returning_st<MockallF>(&mut self,
                        __mockall_f: MockallF) -> &mut Expectation #tg
                        where MockallF: FnMut(#(#argty, )*) -> #output + 'static
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .returning_st(__mockall_f)
                    }

                    /// Just like
                    /// [`Expectation::times`](struct.Expectation.html#method.times)
                    #v fn times<MockallR>(&mut self, __mockall_r: MockallR)
                        -> &mut Expectation #tg
                        where MockallR: Into<::mockall::TimesRange>
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .times(__mockall_r)
                    }

                    /// Just like
                    /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
                    #[deprecated(since = "0.3.0", note = "Use times instead")]
                    #v fn times_any(&mut self) -> &mut Expectation #tg {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .times(..)
                    }

                    /// Just like
                    /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
                    #[deprecated(since = "0.3.0", note = "Use times instead")]
                    #v fn times_range(&mut self, __mockall_range: Range<usize>)
                        -> &mut Expectation #tg
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .times(__mockall_range)
                    }

                    /// Just like
                    /// [`Expectation::with`](struct.Expectation.html#method.with)
                    #v fn with<#with_generics> (&mut self, #with_args)
                        -> &mut Expectation #tg
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .with(#(#argnames, )*)
                    }

                    /// Just like
                    /// [`Expectation::withf`](struct.Expectation.html#method.withf)
                    #v fn withf<MockallF>(&mut self, __mockall_f: MockallF) -> &mut Expectation #tg
                        where MockallF: Fn(#(&#predty, )*) -> bool + Send + 'static
                    {
                        self.guard.store.get_mut(
                                &::mockall::Key::new::<(#(#argty, )*)>()
                            ).unwrap()
                            .downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .0[self.i]
                            .withf(__mockall_f)
                    }
                }
                #context_ts
            )
        }
    }
}

/// For methods that take &self and return a reference
pub(crate) struct RefExpectation<'a> {
    common: Common<'a>,
}

impl<'a> RefExpectation<'a> {
    fn common(&self) -> &Common {&self.common}
    fn expectation(&self, em_ts: TokenStream) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, _wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predexprs = &self.common.predexprs;
        let v = &self.common.vis;
        quote!(
            /// Expectation type for methods taking a `&self` argument and
            /// returning immutable references.  This is the type returned by
            /// the `expect_*` methods.
            #v struct Expectation #ig {
                common: Common #tg,
                rfunc: Rfunc #tg,
            }

            impl #ig Expectation #tg {
                #v fn call(&self, #(#argnames: #argty,)* ) -> &#output {
                    self.common.call(#(#predexprs, )*);
                    self.rfunc.call()
                }

                /// Return a reference to a constant value from the `Expectation`
                #v fn return_const(&mut self, __mockall_o: #output)
                    -> &mut Self
                {
                    mem::replace(&mut self.rfunc, Rfunc::Const(__mockall_o));
                    self
                }

                #em_ts
            }

            impl #ig Default for Expectation #tg
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        rfunc:Rfunc::default()
                    }
                }
            }
        )
    }

    fn expectations_methods(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, _wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predexprs = &self.common.predexprs;
        let v = &self.common.vis;
        quote!(
            impl #ig Expectations #tg {
                /// Simulate calling the real method.  Every current expectation
                /// will be checked in FIFO order and the first one with
                /// matching arguments will be used.
                #v fn call(&self, #(#argnames: #argty,)* ) -> Option<&#output> {
                    self.0.iter()
                        .find(|__mockall_e|
                              __mockall_e.matches(#(#predexprs, )*) &&
                              (!__mockall_e.is_done() || self.0.len() == 1))
                        .map(move |__mockall_e|
                             __mockall_e.call(#(#argnames, )*)
                        )
                }
            }
            // The Senc + Sync are required for downcast, since Expectation
            // stores an Option<#output>
            impl #ig ::mockall::AnyExpectations for Expectations #tg
                    where #output: Send + Sync
            {}
        )
    }

    fn extra_uses(&self) -> TokenStream { TokenStream::new() }

    fn generic_expectations_methods(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let tbf = tg.as_turbofish();
        let v = &self.common.vis;

        quote!(
            impl GenericExpectations {
                /// Simulating calling the real method.
                #v fn call #ig (&self, #(#argnames: #argty,)*)
                    -> Option<&#output>
                {
                    self.store.get(&::mockall::Key::new::<(#(#argty, )*)>())
                        .map(|__mockall_e| {
                            __mockall_e.downcast_ref::<Expectations #tg>()
                            .unwrap()
                            .call(#(#argnames, )*)
                        // TODO: use Option::flatten once it stabilizes
                        // https://github.com/rust-lang/rust/issues/60258
                        }).and_then(std::convert::identity)
                }

                /// Create a new Expectation.
                #v fn expect #ig (&mut self)
                    -> &mut Expectation #tg
                    #wc
                    where #output: Send + Sync
                {
                    self.store.entry(::mockall::Key::new::<(#(#argty, )*)>())
                        .or_insert_with(||
                            Box::new(Expectations #tbf ::new())
                        ).downcast_mut::<Expectations #tg>()
                        .unwrap()
                        .expect()
                }
            }
        )
    }

    fn new(common: Common<'a>) -> Self {
        RefExpectation { common, }
    }

    fn rfunc(&self) -> TokenStream {
        let fn_params = &self.common.fn_params;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        quote!(
            enum Rfunc #ig #wc {
                Default(Option<#output>),
                Const(#output),
                // Prevent "unused type parameter" errors Surprisingly,
                // PhantomData<Fn(generics)> is Send even if generics are not,
                // unlike PhantomData<generics>
                _Phantom(Mutex<Box<dyn Fn(#fn_params) -> () + Send>>)
            }

            impl #ig  Rfunc #tg #wc {
                fn call(&self) -> &#output {
                    match self {
                        Rfunc::Default(Some(ref __mockall_o)) => {
                            __mockall_o
                        },
                        #[cfg(feature = "nightly")]
                        Rfunc::Default(None) => {
                            panic!("Can only return default values for types that impl std::Default");
                        },
                        #[cfg(not(feature = "nightly"))]
                        Rfunc::Default(None) => {
                            panic!("Returning default values requires the \"nightly\" feature");
                        },
                        Rfunc::Const(ref __mockall_o) => {
                            __mockall_o
                        },
                        Rfunc::_Phantom(_) => unreachable!()
                    }
                }
            }

            impl #ig std::default::Default for Rfunc #tg #wc
            {
                fn default() -> Self {
                    use ::mockall::ReturnDefault;
                    Rfunc::Default(::mockall::DefaultReturner::<#output>
                                ::maybe_return_default())
                }
            }
        )
    }

    fn static_method_methods(&self, _with_generics: &TokenStream,
        _with_args: &TokenStream) -> TokenStream
    {
        TokenStream::new()
    }
}

/// For methods that take &mut self and return a reference
pub(crate) struct RefMutExpectation<'a> {
    common: Common<'a>,
}

impl<'a> RefMutExpectation<'a> {
    fn common(&self) -> &Common {&self.common}
    fn expectation(&self, em_ts: TokenStream) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, _wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predexprs = &self.common.predexprs;
        let v = &self.common.vis;
        quote!(
            /// Expectation type for methods taking a `&mut self` argument and
            /// returning references.  This is the type returned by the
            /// `expect_*` methods.
            #v struct Expectation #ig {
                common: Common #tg,
                rfunc: Rfunc #tg
            }

            impl #ig Expectation #tg {
                /// Simulating calling the real method for this expectation
                #v fn call_mut(&mut self, #(#argnames: #argty, )*)
                    -> &mut #output
                {
                    self.common.call(#(#predexprs, )*);
                    self.rfunc.call_mut(#(#argnames, )*)
                }

                /// Convenience method that can be used to supply a return value
                /// for a `Expectation`.  The value will be returned by mutable
                /// reference.
                #v fn return_var(&mut self, __mockall_o: #output) -> &mut Self
                {
                    mem::replace(&mut self.rfunc, Rfunc::Var(__mockall_o));
                    self
                }

                /// Supply a closure that the `Expectation` will use to create its
                /// return value.  The return value will be returned by mutable
                /// reference.
                #v fn returning<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Self
                    where MockallF: FnMut(#(#argty, )*) -> #output + Send + Sync + 'static
                {
                    mem::replace(&mut self.rfunc,
                                 Rfunc::Mut(Box::new(__mockall_f), None));
                    self
                }

                /// Single-threaded version of [`returning`](#method.returning).
                /// Can be used when the argument or return type isn't `Send`.
                #v fn returning_st<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Self
                    where MockallF: FnMut(#(#argty, )*) -> #output + 'static
                {
                    let mut __mockall_fragile = ::mockall::Fragile::new(__mockall_f);
                    let __mockall_fmut = move |#(#argnames, )*| {
                        (__mockall_fragile.get_mut())(#(#argnames, )*)
                    };
                    mem::replace(&mut self.rfunc,
                                 Rfunc::Mut(Box::new(__mockall_fmut), None));
                    self
                }

                #em_ts
            }
            impl #ig Default for Expectation #tg
            {
                fn default() -> Self {
                    Expectation {
                        common: Common::default(),
                        rfunc: Rfunc::default()
                    }
                }
            }
        )
    }

    fn expectations_methods(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, _wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let predexprs = &self.common.predexprs;
        let v = &self.common.vis;
        quote!(
            impl #ig Expectations #tg {
                /// Simulate calling the real method.  Every current expectation
                /// will be checked in FIFO order and the first one with
                /// matching arguments will be used.
                #v fn call_mut(&mut self, #(#argnames: #argty, )* )
                    -> Option<&mut #output>
                {
                    let __mockall_n = self.0.len();
                    self.0.iter_mut()
                        .find(|__mockall_e|
                              __mockall_e.matches(#(#predexprs, )*) &&
                              (!__mockall_e.is_done() || __mockall_n == 1))
                        .map(move |__mockall_e|
                             __mockall_e.call_mut(#(#argnames, )*)
                        )
                }
            }
            // The Senc + Sync are required for downcast, since Expectation
            // stores an Option<#output>
            impl #ig
                ::mockall::AnyExpectations for Expectations #tg
                where #output: Send + Sync
            {}
        )
    }
    fn extra_uses(&self) -> TokenStream { TokenStream::new() }

    fn generic_expectations_methods(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        let tbf = tg.as_turbofish();
        let v = &self.common.vis;

        quote!(
            impl GenericExpectations {
                /// Simulating calling the real method.
                #v fn call_mut #ig (&mut self, #(#argnames: #argty, )* )
                    -> Option<&mut #output>
                {
                    self.store.get_mut(&::mockall::Key::new::<(#(#argty, )*)>())
                        .map(|__mockall_e| {
                            __mockall_e.downcast_mut::<Expectations #tg>()
                            .unwrap()
                            .call_mut(#(#argnames, )*)
                        // TODO: use Option::flatten once it stabilizes
                        // https://github.com/rust-lang/rust/issues/60258
                        }).and_then(std::convert::identity)
                }

                /// Create a new Expectation.
                #v fn expect #ig (&mut self) -> &mut Expectation #tg #wc
                    where #output: Send + Sync
                {
                    self.store.entry(::mockall::Key::new::<(#(#argty, )*)>())
                        .or_insert_with(||
                            Box::new(Expectations #tbf ::new())
                        ).downcast_mut::<Expectations #tg>()
                        .unwrap()
                        .expect()
                }
            }
        )
    }

    fn new(common: Common<'a>) -> Self {
        RefMutExpectation { common, }
    }

    fn rfunc(&self) -> TokenStream {
        let argnames = &self.common.argnames;
        let argty = &self.common.argty;
        let fn_params = &self.common.fn_params;
        let (ig, tg, wc) = self.common.egenerics.split_for_impl();
        let output = &self.common.output;
        quote!(
            enum Rfunc #ig #wc {
                Default(Option<#output>),
                Mut((Box<dyn FnMut(#(#argty, )*) -> #output + Send + Sync>),
                    Option<#output>),
                Var(#output),
                // Prevent "unused type parameter" errors Surprisingly,
                // PhantomData<Fn(generics)> is Send even if generics are not,
                // unlike PhantomData<generics>
                _Phantom(Mutex<Box<dyn Fn(#fn_params) -> () + Send>>)
            }

            impl #ig  Rfunc #tg #wc {
                fn call_mut(&mut self, #(#argnames: #argty, )*) -> &mut #output {
                    match self {
                        Rfunc::Default(Some(ref mut __mockall_o)) => {
                            __mockall_o
                        },
                        #[cfg(feature = "nightly")]
                        Rfunc::Default(None) => {
                            panic!("Can only return default values for types that impl std::Default");
                        },
                        #[cfg(not(feature = "nightly"))]
                        Rfunc::Default(None) => {
                            panic!("Returning default values requires the \"nightly\" feature");
                        },
                        Rfunc::Mut(ref mut __mockall_f, ref mut __mockall_o) =>
                        {
                            *__mockall_o = Some(__mockall_f(#(#argnames, )*));
                            if let Some(ref mut __mockall_o2) = __mockall_o {
                                __mockall_o2
                            } else {
                                unreachable!()
                            }
                        },
                        Rfunc::Var(ref mut __mockall_o) => {
                            __mockall_o
                        },
                        Rfunc::_Phantom(_) => unreachable!()
                    }
                }
            }

            impl #ig std::default::Default for Rfunc #tg #wc
            {
                fn default() -> Self {
                    use ::mockall::ReturnDefault;
                    Rfunc::Default(::mockall::DefaultReturner::<#output>
                                ::maybe_return_default())
                }
            }
        )
    }

    fn static_method_methods(&self, _with_generics: &TokenStream,
        _with_args: &TokenStream) -> TokenStream
    {
        TokenStream::new()
    }
}
