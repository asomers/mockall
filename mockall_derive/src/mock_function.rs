// vim: tw=80
use super::*;

use quote::ToTokens;

/// Build a MockFunction.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Builder<'a> {
    attrs: &'a [Attribute],
    call_levels: Option<i32>,
    levels: i32,
    parent: Option<&'a Ident>,
    sig: &'a Signature,
    struct_: Option<&'a Ident>,
    vis: &'a Visibility
}

impl<'a> Builder<'a> {
    pub fn attrs(&mut self, attrs: &'a[Attribute]) -> &mut Self {
        self.attrs = attrs;
        self
    }

    pub fn build(self) -> MockFunction {
        let egenerics = Generics::default();    // TODO
        let mut argnames = Vec::new();
        let mut argty = Vec::new();
        let mut is_static = true;
        let mut predexprs = Vec::new();
        let mut predty = Vec::new();
        let mut refpredty = Vec::new();
        for fa in self.sig.inputs.iter() {
            if let FnArg::Typed(pt) = fa {
                let argname = (*pt.pat).clone();
                let aty = supersuperfy(&pt.ty, self.levels);
                if let Type::Reference(ref tr) = aty {
                    predexprs.push(quote!(#argname));
                    predty.push((*tr.elem).clone());
                    refpredty.push(aty.clone());
                } else {
                    predexprs.push(quote!(&#argname));
                    predty.push(aty.clone());
                    let tr = TypeReference {
                        and_token: Token![&](Span::call_site()),
                        lifetime: None,
                        mutability: None,
                        elem: Box::new(aty.clone())
                    };
                    refpredty.push(Type::Reference(tr));
                };
                argnames.push(argname);
                argty.push(aty.clone());
            } else {
                is_static = false;
                ()    // Strip out the "&self" argument
            }
        }
        let output = supersuperfy(&match self.sig.output.clone() {
            ReturnType::Default => Type::Tuple(TypeTuple{
                paren_token: token::Paren(Span::call_site()),
                elems: Punctuated::new()
            }),
            ReturnType::Type(_, ty) => (*ty).clone()
        }, self.levels);
        let (mut egenerics, alifetimes, rlifetimes) = split_lifetimes(
            self.sig.generics.clone(),
            &self.sig.inputs,
            &self.sig.output
        );
        let fn_params = Punctuated::<Ident, Token![,]>::from_iter(
            egenerics.type_params().map(|tp| tp.ident.clone())
        );
        let call_levels = self.call_levels.unwrap_or(self.levels);
        MockFunction {
            alifetimes,
            argnames,
            argty,
            attrs: self.attrs.to_vec(),
            call_vis: expectation_visibility(self.vis, call_levels),
            egenerics,
            fn_params,
            is_static,
            mod_ident: self.parent.unwrap().clone(),
            output,
            predexprs,
            predty,
            refpredty,
            rlifetimes,
            sig: self.sig.clone(),
            struct_: self.struct_.cloned(),
            privmod_vis: expectation_visibility(self.vis, self.levels + 1)
        }
    }

    /// How many levels of modules beneath the original function this one is
    /// nested.
    pub fn call_levels(&mut self, levels: i32) -> &mut Self {
        self.call_levels = Some(levels);
        self
    }

    /// How many levels of modules beneath the original function this one's
    /// private module is nested.
    pub fn levels(&mut self, levels: i32) -> &mut Self {
        self.levels = levels;
        self
    }

    /// # Arguments
    ///
    /// * sig:      The signature of the mockable function
    /// * v:        The visibility of the mockable function
    pub fn new(sig: &'a Signature, vis: &'a Visibility) -> Self {
        Builder {
            attrs: &[],
            levels: 0,
            call_levels: None,
            parent: None,
            sig,
            struct_: None,
            vis
        }
    }

    /// Supply the name of the parent module
    pub fn parent(&mut self, ident: &'a Ident) -> &mut Self {
        self.parent = Some(ident);
        self
    }

    /// Supply the name of the parent struct, if any
    pub fn struct_(&mut self, ident: &'a Ident) -> &mut Self {
        self.struct_= Some(ident);
        self
    }
}

pub(crate) struct MockFunction {
    /// Lifetime Generics of the mocked method that relate to the arguments but
    /// not the return value
    alifetimes: Generics,
    /// Names of the method arguments
    argnames: Vec<Pat>,
    /// Types of the method arguments
    argty: Vec<Type>,
    /// any attributes on the original function, like #[inline]
    attrs: Vec<Attribute>,
    /// Visibility of the mock function itself
    call_vis: Visibility,
    /// Type Generics of the Expectation object
    egenerics: Generics,
    /// The mock function's generic types as a list of types
    fn_params: Punctuated<Ident, Token![,]>,
    /// Is this for a static method or free function?
    is_static: bool,
    /// name of the function's parent module
    mod_ident: Ident,
    /// Output type of the Method, supersuperfied.
    output: Type,
    /// Expressions that create the predicate arguments from the call arguments
    predexprs: Vec<TokenStream>,
    /// Types used for Predicates.  Will be almost the same as args, but every
    /// type will be a non-reference type.
    predty: Vec<Type>,
    /// References to every type in `predty`.
    refpredty: Vec<Type>,
    /// Lifetime Generics of the mocked method that relate to the return value
    rlifetimes: Generics,
    /// The signature of the mockable function
    sig: Signature,
    /// Name of the parent structure, if any
    struct_: Option<Ident>,
    /// Visibility of the expectation and its methods
    privmod_vis: Visibility
}

impl MockFunction {
    /// Return the mock function itself
    pub fn call(&self) -> impl ToTokens {
        let argnames = &self.argnames;
        let attrs = self.format_attrs(true);
        let (ig, tg, wc) = self.egenerics.split_for_impl();
        let tbf = tg.as_turbofish();
        let name = self.name();
        let fn_docstr = format!("Mock version of the `{}` function", name);
        let no_match_msg = if let Some(s) = &self.struct_ {
            format!("{}::{}: No matching expectation found", s, name)
        } else {
            format!("{}::{}: No matching expectation found", self.mod_ident,
                    self.name())
        };
        let sig = &self.sig;
        let vis = &self.call_vis;
        if self.is_static {
            let inner_mod_ident = self.inner_mod_ident();
            quote!(
                #attrs
                #[doc = #fn_docstr]
                #vis #sig {
                    {
                        let __mockall_guard = #inner_mod_ident::EXPECTATIONS
                            .lock().unwrap();
                        /*
                         * TODO: catch panics, then gracefully release the mutex
                         * so it won't be poisoned.  This requires bounding any
                         * generic parameters with UnwindSafe
                         */
                        /* std::panic::catch_unwind(|| */
                        __mockall_guard.call(#(#argnames),*)
                        /*)*/
                    }.expect(#no_match_msg)
                }
            )
        } else {
            // TODO:
            // * call vs call_mut
            // * call_exprs (declosurefy)
            // * substructs
            // * Add fn_docstr
            quote!(
                #attrs
                #vis #sig {
                    self.#name.call#tbf(#(#argnames),*)
                    .expect(#no_match_msg)
                }

            )
        }
    }

    /// Return this method's contribution to its parent's checkpoint method
    pub fn checkpoint(&self) -> impl ToTokens {
        let inner_mod_ident = self.inner_mod_ident();
        if self.is_static {
            quote!(
                let __mockall_timeses = #inner_mod_ident::EXPECTATIONS.lock()
                    .unwrap()
                    .checkpoint()
                    .collect::<Vec<_>>();
            )
        } else {
            let attrs = self.format_attrs(false);
            let name = &self.name();
            quote!(#attrs { self.#name.checkpoint(); })
        }
    }

    /// Return a function that creates a Context object for this function
    pub fn context_fn(&self) -> impl ToTokens {
        let context_docstr = format!("Return a Context object used to hold the expectations for `{}`",
            self.name());
        let context_ident = format_ident!("{}_context", self.name());
        let inner_mod_ident = self.inner_mod_ident();
        let v = &self.call_vis;
        quote!(
            #[doc = #context_docstr]
            #v fn #context_ident() -> #inner_mod_ident::Context
            {
                #inner_mod_ident::Context::default()
            }
        )
    }

    /// Generate code for the expect_ method
    ///
    /// # Arguments
    ///
    /// * `modname`:    Name of the parent struct's private module
    // Supplying modname is an unfortunately hack.  Ideally MockFunction
    // wouldn't need to know that.
    pub fn expect(&self, modname: &Ident) -> impl ToTokens {
        let attrs = self.format_attrs(false);
        let name = self.name();
        let expect_ident = format_ident!("expect_{}", &name);
        let expectation_obj = self.expectation_obj();
        let inner_mod_ident = self.inner_mod_ident();
        let (ig, tg, wc) = self.egenerics.split_for_impl();
        let tbf = tg.as_turbofish();
        let vis = &self.call_vis;

        #[cfg(not(feature = "nightly_derive"))]
        let must_use = quote!(#[must_use =
                "Must set return value when not using the \"nightly\" feature"
            ]);
        #[cfg(feature = "nightly_derive")]
        let must_use = quote!();

        // TODO:
        // * substructs
        // * Fancy generics alternatives
        // * Fix "#mod_ident" in the doc string
        quote!(
            #must_use
            #attrs
            /// Create an
            /// [`Expectation`](#mod_ident/ident/struct.Expectation.html) for
            /// mocking the `ident` method
            #vis fn #expect_ident #ig(&mut self)
               -> &mut #modname::#expectation_obj
               #wc
            {
                self.#name.expect#tbf()
            }
        )
    }

    /// Return the name of this function's expecation object
    pub fn expectation_obj(&self) -> impl ToTokens {
        let inner_mod_ident = self.inner_mod_ident();
        quote!(#inner_mod_ident::Expectation)
    }

    /// Return the name of this function's expecations object
    pub fn expectations_obj(&self) -> impl ToTokens {
        let inner_mod_ident = self.inner_mod_ident();
        quote!(#inner_mod_ident::Expectations)
    }

    pub fn format_attrs(&self, include_docs: bool) -> impl ToTokens {
        let mut out = TokenStream::new();
        for attr in &self.attrs {
            let is_doc = attr.path.get_ident().map(|i| i == "doc").unwrap_or(false);
            if !is_doc || include_docs {
                attr.to_tokens(&mut out);
            }
        }
        out
    }

    // TODO: return a Syn object instead of a TokenStream
    fn hrtb(&self) -> TokenStream {
        if self.alifetimes.params.is_empty() {
            TokenStream::default()
        } else {
            let ig = &self.alifetimes;
            quote!(for #ig)
        }
    }

    // TODO: choose a better name
    fn ident_str(&self) -> String {
        if let Some(si) = &self.struct_ {
            format!("{}::{}", si, self.name())
        } else {
            format!("{}", self.name())
        }
    }

    fn inner_mod_ident(&self) -> Ident {
        format_ident!("__{}", &self.name())
    }

    pub fn name(&self) -> &Ident {
        &self.sig.ident
    }

    /// Generate code for this function's private module
    pub fn priv_module(&self) -> impl ToTokens {
        let argnames = &self.argnames;
        let attrs = self.format_attrs(false);
        let call = self.call();
        let common = &Common{f: self};
        let context = &Context{f: self};
        // TODO: non-Static expectations
        let expectation = &StaticExpectation{f: self};
        let expectations = &StaticExpectations{f: self};
        let fn_ident = &self.name();
        let fn_docstr = format!("Mock version of the `{}` function", fn_ident);
        // TODO: ExpectationGuard for generic methods
        let guard = &ConcreteExpectationGuard{f: self};
        let matcher = &Matcher{f: self};
        let mod_ident = &self.mod_ident;
        let std_mutexguard = if self.is_static {
            quote!(use ::std::sync::MutexGuard;)
        } else {
            quote!()
        };
        let inner_mod_ident = self.inner_mod_ident();
        let no_match_msg = format!("{}::{}: No matching expectation found",
            mod_ident, fn_ident);
        let orig_signature = &self.sig;
        let rfunc = &Rfunc{f: self};
        quote!(
            #attrs
            #[allow(missing_docs)]
            pub mod #inner_mod_ident {
                use super::*;
                use ::mockall::CaseTreeExt;
                #std_mutexguard
                use ::std::{
                    mem,
                    ops::{DerefMut, Range},
                    sync::Mutex
                };
                #rfunc
                #matcher
                #common
                #expectation
                #expectations
                #guard
                #context
            }
        )
    }
}

/// Holds parts of the expectation that are common for all output types
struct Common<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for Common<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let argnames = &self.f.argnames;
        let predty = &self.f.predty;
        let hrtb = self.f.hrtb();
        let ident_str = self.f.ident_str();
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let lg = &self.f.alifetimes;
        let refpredty = &self.f.refpredty;
        let with_generics_idents = (0..self.f.predty.len())
            .map(|i| format_ident!("MockallMatcher{}", i))
            .collect::<Vec<_>>();
        let with_generics = TokenStream::from_iter(
            with_generics_idents.iter().zip(self.f.predty.iter())
            .map(|(id, mt)|
                quote!(#id: #hrtb ::mockall::Predicate<#mt> + Send + 'static, )
            )
        );
        let with_args = TokenStream::from_iter(
            self.f.argnames.iter().zip(with_generics_idents.iter())
            .map(|(argname, id)| quote!(#argname: #id, ))
        );
        let boxed_withargs = TokenStream::from_iter(
            argnames.iter().map(|aa| quote!(Box::new(#aa), ))
        );
        quote!(
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
                        times: ::mockall::Times::default()
                    }
                }
            }

            impl #ig Common #tg #wc {
                fn call(&self) {
                    self.times.call()
                        .unwrap_or_else(|m| {
                            let desc = format!("{}",
                                               self.matcher.lock().unwrap());
                            panic!("{}: Expectation({}) {}", #ident_str, desc,
                                m);
                        });
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

                fn matches #lg (&self, #( #argnames: &#predty, )*) -> bool {
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
                    *__mockall_guard.deref_mut() =
                        Matcher::Pred(Box::new((#boxed_withargs)));
                }

                fn withf<MockallF>(&mut self, __mockall_f: MockallF)
                    where MockallF: #hrtb Fn(#( #refpredty, )*)
                                    -> bool + Send + 'static
                {
                    let mut __mockall_guard = self.matcher.lock().unwrap();
                    *__mockall_guard.deref_mut() =
                         Matcher::Func(Box::new(__mockall_f));
                }

                fn withf_st<MockallF>(&mut self, __mockall_f: MockallF)
                    where MockallF: #hrtb Fn(#( #refpredty, )*)
                                    -> bool + 'static
                {
                    let mut __mockall_guard = self.matcher.lock().unwrap();
                    *__mockall_guard.deref_mut() =
                         Matcher::FuncST(
                             ::mockall::Fragile::new(Box::new(__mockall_f))
                        );
                }

                fn verify_sequence(&self) {
                    if let Some(__mockall_handle) = &self.seq_handle {
                        __mockall_handle.verify()
                    }
                }
            }

            impl #ig Drop for Common #tg #wc {
                fn drop(&mut self) {
                    if !::std::thread::panicking() && !self.times.is_satisfied()
                    {
                        let desc = format!("{}", self.matcher.lock().unwrap());
                        panic!("{}: Expectation({}) called fewer than {} times",
                               #ident_str,
                               desc,
                               self.times.minimum());
                    }
                }
            }
        ).to_tokens(tokens);
    }
}

/// Generates methods that are common for all Expectation types
struct CommonExpectationMethods<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for CommonExpectationMethods<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let argnames = &self.f.argnames;
        let hrtb = self.f.hrtb();
        let lg = &self.f.alifetimes;
        let predty = &self.f.predty;
        let with_generics_idents = (0..self.f.predty.len())
            .map(|i| format_ident!("MockallMatcher{}", i))
            .collect::<Vec<_>>();
        let with_generics = TokenStream::from_iter(
            with_generics_idents.iter().zip(self.f.predty.iter())
            .map(|(id, mt)|
                quote!(#id: #hrtb ::mockall::Predicate<#mt> + Send + 'static, )
            )
        );
        let with_args = TokenStream::from_iter(
            self.f.argnames.iter().zip(with_generics_idents.iter())
            .map(|(argname, id)| quote!(#argname: #id, ))
        );
        let v = &self.f.privmod_vis;
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
            fn matches #lg (&self, #(#argnames: &#predty, )*) -> bool {
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
            /// This is equivalent to calling [`with`](#method.with) with a
            /// function argument, like `with(predicate::function(f))`.
            #v fn withf<MockallF>(&mut self, __mockall_f: MockallF) -> &mut Self
                where MockallF: #hrtb Fn(#(&#predty, )*)
                                -> bool + Send + 'static
            {
                self.common.withf(__mockall_f);
                self
            }

            /// Single-threaded version of [`withf`](#method.withf).
            /// Can be used when the argument type isn't `Send`.
            #v fn withf_st<MockallF>(&mut self, __mockall_f: MockallF) -> &mut Self
                where MockallF: #hrtb Fn(#(&#predty, )*)
                                -> bool + 'static
            {
                self.common.withf_st(__mockall_f);
                self
            }
        ).to_tokens(tokens);
    }
}

/// Holds the moethods of the Expectations object that are common for all
/// Expectation types
struct CommonExpectationsMethods<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for CommonExpectationsMethods<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let v = &self.f.privmod_vis;
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
        ).to_tokens(tokens);
    }
}

/// The ExpectationGuard structure for static methods with no generic types
struct ConcreteExpectationGuard<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for ConcreteExpectationGuard<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if !self.f.is_static {
            return;
        }

        let argnames = &self.f.argnames;
        let argty = &self.f.argty;
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let ltdef = LifetimeDef::new(
            Lifetime::new("'__mockall_lt", Span::call_site())
        );
        let mut e_generics = self.f.egenerics.clone();
        e_generics.lt_token.get_or_insert(<Token![<]>::default());
        e_generics.params.push(GenericParam::Lifetime(ltdef));
        e_generics.gt_token.get_or_insert(<Token![>]>::default());
        let ei_generics = merge_generics(&e_generics, &self.f.rlifetimes);
        let (e_ig, e_tg, e_wc) = e_generics.split_for_impl();
        let (ei_ig, _, _) = ei_generics.split_for_impl();
        let fn_params = &self.f.fn_params;
        let hrtb = self.f.hrtb();
        let output = &self.f.output;
        let predty = &self.f.predty;
        let tbf = tg.as_turbofish();
        let with_generics_idents = (0..self.f.predty.len())
            .map(|i| format_ident!("MockallMatcher{}", i))
            .collect::<Vec<_>>();
        let with_generics = TokenStream::from_iter(
            with_generics_idents.iter().zip(self.f.predty.iter())
            .map(|(id, mt)|
                quote!(#id: #hrtb ::mockall::Predicate<#mt> + Send + 'static, )
            )
        );
        let with_args = TokenStream::from_iter(
            self.f.argnames.iter().zip(with_generics_idents.iter())
            .map(|(argname, id)| quote!(#argname: #id, ))
        );
        let v = &self.f.privmod_vis;
        quote!(
            ::mockall::lazy_static! {
                #[doc(hidden)]
                #v static ref EXPECTATIONS:
                    ::std::sync::Mutex<Expectations #tg> =
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

            impl #ei_ig ExpectationGuard #e_tg #e_wc
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
                /// [`Expectation::return_const`](struct.Expectation.html#method.return_const)
                #v fn return_const<MockallOutput>
                (&mut self, __mockall_c: MockallOutput)
                    -> &mut Expectation #tg
                    where MockallOutput: Clone + Into<#output> + Send + 'static
                {
                    self.guard.0[self.i].return_const(__mockall_c)
                }

                /// Just like
                /// [`Expectation::returning`](struct.Expectation.html#method.returning)
                #v fn returning<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Expectation #tg
                    where MockallF: #hrtb FnMut(#(#argty, )*)
                        -> #output + Send + 'static
                {
                    self.guard.0[self.i].returning(__mockall_f)
                }

                /// Just like
                /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
                #v fn return_once<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Expectation #tg
                    where MockallF: #hrtb FnOnce(#(#argty, )*)
                                    -> #output + Send + 'static
                {
                    self.guard.0[self.i].return_once(__mockall_f)
                }

                /// Just like
                /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
                #v fn returning_st<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Expectation #tg
                    where MockallF: #hrtb FnMut(#(#argty, )*)
                                    -> #output + 'static
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
                    where MockallF: #hrtb Fn(#(&#predty, )*)
                                    -> bool + Send + 'static
                {
                    self.guard.0[self.i].withf(__mockall_f)
                }

                /// Just like
                /// [`Expectation::withf_st`](struct.Expectation.html#method.withf_st)
                #v fn withf_st<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Expectation #tg
                    where MockallF: #hrtb Fn(#(&#predty, )*)
                                    -> bool + 'static
                {
                    self.guard.0[self.i].withf_st(__mockall_f)
                }
            }
        ).to_tokens(tokens);
    }
}

/// Generates Context, which manages the context for expectations of static
/// methods.
struct Context<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for Context<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if !self.f.is_static {
            return;
        }

        let ltdef = LifetimeDef::new(
            Lifetime::new("'__mockall_lt", Span::call_site())
        );
        let mut e_generics = self.f.egenerics.clone();
        e_generics.lt_token.get_or_insert(<Token![<]>::default());
        e_generics.params.push(GenericParam::Lifetime(ltdef));
        e_generics.gt_token.get_or_insert(<Token![>]>::default());
        let ei_generics = merge_generics(&e_generics, &self.f.rlifetimes);
        let (e_ig, e_tg, e_wc) = e_generics.split_for_impl();
        let gd = Generics::default();
        let (s_ig, s_tg, s_wc) = gd.split_for_impl();   // TODO: generic structs
        let mut meth_generics = gd.clone();
        let ltdef = LifetimeDef::new(
            Lifetime::new("'__mockall_lt", Span::call_site())
        );
        meth_generics.params.push(GenericParam::Lifetime(ltdef.clone()));
        let (meth_ig, _meth_tg, meth_wc) = meth_generics.split_for_impl();
        let ctx_fn_params = Punctuated::<Ident, Token![,]>::new();  // TODO
        let v = &self.f.privmod_vis;

        #[cfg(not(feature = "nightly_derive"))]
        let must_use = quote!(#[must_use =
                "Must set return value when not using the \"nightly\" feature"
            ]);
        #[cfg(feature = "nightly_derive")]
        let must_use = quote!();

        quote!(
            /// Manages the context for expectations of static methods.
            ///
            /// Expectations on this method will be validated and cleared when
            /// the `Context` object drops.  The `Context` object does *not*
            /// provide any form of synchronization, so multiple tests that set
            /// expectations on the same static method must provide their own.
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
                /// Verify that all current expectations for this method are
                /// satisfied and clear them.
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

                /// Create a new expectation for this method.
                #must_use
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
        ).to_tokens(tokens);
    }
}

struct Matcher<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for Matcher<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let argnames = &self.f.argnames;
        let braces = argnames.iter()
            .fold(String::new(), |mut acc, _argname| {
                if acc.is_empty() {
                    acc.push_str("{}");
                } else {
                    acc.push_str(", {}");
                }
                acc
            });
        let fn_params = &self.f.fn_params;
        let hrtb = self.f.hrtb();
        let indices = (0..argnames.len())
            .map(|i| {
                syn::Index::from(i)
            }).collect::<Vec<_>>();
        let lg = &self.f.alifetimes;
        let pred_matches = TokenStream::from_iter(
            argnames.iter().enumerate()
            .map(|(i, argname)| {
                let idx = syn::Index::from(i);
                quote!(__mockall_pred.#idx.eval(#argname),)
            })
        );
        let preds = TokenStream::from_iter(
            self.f.predty.iter().map(|t|
                quote!(Box<dyn #hrtb ::mockall::Predicate<#t> + Send>,)
            )
        );
        let predty = &self.f.predty;
        let refpredty = &self.f.refpredty;
        quote!(
            enum Matcher #ig #wc {
                Always,
                Func(Box<dyn #hrtb Fn(#( #refpredty, )*) -> bool + Send>),
                // Version of Matcher::Func for closures that aren't Send
                FuncST(::mockall::Fragile<Box<dyn #hrtb Fn(#( #refpredty, )*) -> bool>>),
                Pred(Box<(#preds)>),
                // Prevent "unused type parameter" errors
                // Surprisingly, PhantomData<Fn(generics)> is Send even if
                // generics are not, unlike PhantomData<generics>
                _Phantom(Box<dyn Fn(#fn_params) -> () + Send>)
            }
            impl #ig Matcher #tg #wc {
                fn matches #lg (&self, #( #argnames: &#predty, )*) -> bool {
                    match self {
                        Matcher::Always => true,
                        Matcher::Func(__mockall_f) =>
                            __mockall_f(#(#argnames, )*),
                        Matcher::FuncST(__mockall_f) =>
                            (__mockall_f.get())(#(#argnames, )*),
                        Matcher::Pred(__mockall_pred) =>
                            [#pred_matches]
                            .iter()
                            .all(|__mockall_x| *__mockall_x),
                        _ => unreachable!()
                    }
                }
            }

            impl #ig Default for Matcher #tg #wc {
                #[allow(unused_variables)]
                fn default() -> Self {
                    Matcher::Always
                }
            }

            impl #ig ::std::fmt::Display for Matcher #tg #wc {
                fn fmt(&self, __mockall_fmt: &mut ::std::fmt::Formatter<'_>)
                    -> ::std::fmt::Result
                {
                    match self {
                        Matcher::Always => write!(__mockall_fmt, "<anything>"),
                        Matcher::Func(_) => write!(__mockall_fmt, "<function>"),
                        Matcher::FuncST(_) => write!(__mockall_fmt, "<single threaded function>"),
                        Matcher::Pred(__mockall_p) => {
                            write!(__mockall_fmt, #braces,
                                #(__mockall_p.#indices,)*)
                        }
                        _ => unreachable!(),
                    }
                }
            }
        ).to_tokens(tokens);
    }
}

struct Rfunc<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for Rfunc<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let argnames = &self.f.argnames;
        let argty = &self.f.argty;
        let fn_params = &self.f.fn_params;
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let hrtb = self.f.hrtb();
        let lg = &self.f.alifetimes;
        let output = &self.f.output;
        quote!(
            enum Rfunc #ig #wc {
                Default,
                // Indicates that a `return_once` expectation has already
                // returned
                Expired,
                Mut(Box<dyn #hrtb FnMut(#(#argty, )*) -> #output + Send>),
                // Version of Rfunc::Mut for closures that aren't Send
                MutST(::mockall::Fragile<
                    Box<dyn #hrtb FnMut(#(#argty, )*) -> #output >>
                ),
                Once(Box<dyn #hrtb FnOnce(#(#argty, )*) -> #output + Send>),
                // Version of Rfunc::Once for closure that aren't Send
                OnceST(::mockall::Fragile<
                    Box<dyn #hrtb FnOnce(#(#argty, )*) -> #output>>
                ),
                // Prevent "unused type parameter" errors Surprisingly,
                // PhantomData<Fn(generics)> is Send even if generics are not,
                // unlike PhantomData<generics>
                _Phantom(Box<dyn Fn(#fn_params) -> () + Send>)
            }

            impl #ig  Rfunc #tg #wc {
                fn call_mut #lg (&mut self, #( #argnames: #argty, )* )
                    -> std::result::Result<#output, &'static str>
                {
                    match self {
                        Rfunc::Default => {
                            use ::mockall::ReturnDefault;
                            ::mockall::DefaultReturner::<#output>
                                ::return_default()
                        },
                        Rfunc::Expired => {
                            Err("called twice, but it returns by move")
                        },
                        Rfunc::Mut(__mockall_f) => {
                            Ok(__mockall_f( #(#argnames, )* ))
                        },
                        Rfunc::MutST(__mockall_f) => {
                            Ok((__mockall_f.get_mut())(#(#argnames,)*))
                        },
                        Rfunc::Once(_) => {
                            if let Rfunc::Once(mut __mockall_f) =
                                mem::replace(self, Rfunc::Expired) {
                                Ok(__mockall_f( #(#argnames, )* ))
                            } else {
                                unreachable!()
                            }
                        },
                        Rfunc::OnceST(_) => {
                            if let Rfunc::OnceST(mut __mockall_f) =
                                mem::replace(self, Rfunc::Expired) {
                                Ok((__mockall_f.into_inner())(#(#argnames,)*))
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
        ).to_tokens(tokens);
    }
}

/// An expectation type for functions return a `'static` value
struct StaticExpectation<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for StaticExpectation<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let common_methods = CommonExpectationMethods{f: &self.f};
        let argnames = &self.f.argnames;
        let argty = &self.f.argty;
        let hrtb = self.f.hrtb();
        let ident_str = self.f.ident_str();
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let common_tg = &tg; // TODO: get the generics right
        let lg = &self.f.alifetimes;
        let output = &self.f.output;
        let v = &self.f.privmod_vis;
        quote!(
            /// Expectation type for methods that return a `'static` type.
            /// This is the type returned by the `expect_*` methods.
            #v struct Expectation #ig #wc {
                common: Common #common_tg,
                rfunc: Mutex<Rfunc #tg>,
            }

            impl #ig Expectation #tg #wc {
                /// Call this [`Expectation`] as if it were the real method.
                #[doc(hidden)]
                #v fn call #lg (&self, #(#argnames: #argty, )* ) -> #output
                {
                    self.common.call();
                    self.rfunc.lock().unwrap().call_mut(#(#argnames, )*)
                        .unwrap_or_else(|message| {
                            let desc = format!("{}",
                                self.common.matcher.lock().unwrap());
                            panic!("{}: Expectation({}) {}", #ident_str, desc,
                                   message);
                        })
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

                /// Supply an `FnOnce` closure that will provide the return
                /// value for this Expectation.  This is useful for return types
                /// that aren't `Clone`.  It will be an error to call this
                /// method multiple times.
                #v fn return_once<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Self
                    where MockallF: #hrtb FnOnce(#(#argty, )*)
                                    -> #output + Send + 'static
                {
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        *__mockall_guard.deref_mut() =
                            Rfunc::Once(Box::new(__mockall_f));
                    }
                    self
                }

                /// Single-threaded version of
                /// [`return_once`](#method.return_once).  This is useful for
                /// return types that are neither `Send` nor `Clone`.
                ///
                /// It is a runtime error to call the mock method from a
                /// different thread than the one that originally called this
                /// method.  It is also a runtime error to call the method more
                /// than once.
                #v fn return_once_st<MockallF>(&mut self, __mockall_f:
                                                  MockallF) -> &mut Self
                    where MockallF: #hrtb FnOnce(#(#argty, )*)
                                    -> #output + 'static
                {
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        *__mockall_guard.deref_mut() = Rfunc::OnceST(
                            ::mockall::Fragile::new(Box::new(__mockall_f)));
                    }
                    self
                }

                /// Supply a closure that will provide the return value for this
                /// `Expectation`.  The method's arguments are passed to the
                /// closure by value.
                #v fn returning<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Self
                    where MockallF: #hrtb FnMut(#(#argty, )*)
                                    -> #output + Send + 'static
                {
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        *__mockall_guard.deref_mut() =
                            Rfunc::Mut(Box::new(__mockall_f));
                    }
                    self
                }

                /// Single-threaded version of [`returning`](#method.returning).
                /// Can be used when the argument or return type isn't `Send`.
                ///
                /// It is a runtime error to call the mock method from a
                /// different thread than the one that originally called this
                /// method.
                #v fn returning_st<MockallF>(&mut self, __mockall_f: MockallF)
                    -> &mut Self
                    where MockallF: #hrtb FnMut(#(#argty, )*)
                                    -> #output + 'static
                {
                    {
                        let mut __mockall_guard = self.rfunc.lock().unwrap();
                        *__mockall_guard.deref_mut() = Rfunc::MutST(
                            ::mockall::Fragile::new(Box::new(__mockall_f)));
                    }
                    self
                }

                #common_methods
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
        ).to_tokens(tokens);
    }
}

/// An Expectations type for functions return a `'static` value
struct StaticExpectations<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for StaticExpectations<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let common_methods = CommonExpectationsMethods{f: &self.f};
        let argnames = &self.f.argnames;
        let argty = &self.f.argty;
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let lg = &self.f.alifetimes;
        let output = &self.f.output;
        let predexprs = &self.f.predexprs;
        let v = &self.f.privmod_vis;
        quote!(
            #common_methods
            impl #ig Expectations #tg #wc {
                /// Simulate calling the real method.  Every current expectation
                /// will be checked in FIFO order and the first one with
                /// matching arguments will be used.
                #v fn call #lg (&self, #(#argnames: #argty, )* )
                    -> Option<#output>
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
        ).to_tokens(tokens);
    }
}
