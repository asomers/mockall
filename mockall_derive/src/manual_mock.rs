// vim: tw=80
use super::*;
use quote::ToTokens;
use std::borrow::Borrow;
use syn::parse::{Parse, ParseStream};

pub(crate) struct ManualMock {
    pub(crate) attrs: Vec<syn::Attribute>,
    pub(crate) vis: syn::Visibility,
    pub(crate) name: syn::Ident,
    pub(crate) generics: syn::Generics,
    // The ManualMock struct's inherent methods.  The blocks will all be empty.
    pub(crate) methods: Vec<syn::ImplItemMethod>,
    pub(crate) traits: Vec<syn::ItemTrait>
}

impl ManualMock {
    pub(crate) fn gen(&self) -> TokenStream {
        let mut output = TokenStream::new();
        let mut mock_body = TokenStream::new();
        let mut cp_body = TokenStream::new();
        let mut has_new = false;
        let mock_struct_name = gen_mock_ident(&self.name);
        let mock_mod_ident = gen_mod_ident(&self.name, None);
        let subs = self.traits.iter().map(|trait_| {
            (trait_.ident.to_string(), self.generics.clone())
        }).collect::<Vec<_>>();
        // generate the mock structure
        gen_struct(&self.attrs[..], &mock_struct_name, &self.vis, &self.name,
                   &self.generics, &subs, &self.methods)
            .to_tokens(&mut output);
        // generate sub structures
        for trait_ in self.traits.iter() {
            let mut sub_cp_body = TokenStream::new();
            let sub_mock = format_ident!("{}_{}", &self.name, &trait_.ident);
            let sub_struct = format_ident!("{}_expectations", &trait_.ident);
            let mod_ident = gen_mod_ident(&self.name, Some(&trait_.ident));
            let methods = trait_.items.iter().filter_map(|item| {
                if let syn::TraitItem::Method(m) = item {
                    Some(tim2iim(m, &self.vis))
                } else {
                    None
                }
            }).collect::<Vec<_>>();
            let vis = syn::Visibility::Inherited;
            gen_struct(&[], &mock_struct_name, &vis, &sub_mock,
                       &self.generics, &[], &methods)
                .to_tokens(&mut output);
            let mock_sub_name = gen_mock_ident(&sub_mock);
            for meth in methods {
                has_new |= meth.borrow().sig.ident == "new";
                let generics = merge_generics(&self.generics, &trait_.generics);
                let (_, _, cp) = gen_mock_method(&mock_struct_name,
                                                 Some(&mod_ident),
                                                 &meth.attrs[..],
                                                 &meth.vis, &meth.vis,
                                                 &meth.borrow().sig, None,
                                                 &generics);
                cp.to_tokens(&mut sub_cp_body);
            }
            let (ig, tg, wc) = self.generics.split_for_impl();
            quote!(impl #ig #mock_sub_name #tg #wc {
                /// Validate that all current expectations for all methods have
                /// been satisfied, and discard them.
                pub fn checkpoint(&mut self) {
                    #sub_cp_body
                }
            }).to_tokens(&mut output);
            quote!(self.#sub_struct.checkpoint();).to_tokens(&mut cp_body);
        }
        // generate methods on the mock structure itself
        for meth in self.methods.iter() {
            has_new |= meth.sig.ident == "new";
            let (mm, em, cp) = gen_mock_method(&mock_struct_name,
                                               Some(&mock_mod_ident),
                                               &meth.attrs[..],
                                               &meth.vis, &meth.vis,
                                               &meth.sig, None,
                                               &self.generics);
            // For inherent methods, use the same visibility for the mock and
            // expectation method as for the original.
            mm.to_tokens(&mut mock_body);
            em.to_tokens(&mut mock_body);
            cp.to_tokens(&mut cp_body);
        }
        // generate the mock struct's inherent methods
        quote!(
            /// Validate that all current expectations for all methods have
            /// been satisfied, and discard them.
            pub fn checkpoint(&mut self) {
                #cp_body
            }
        ).to_tokens(&mut mock_body);
        // Add a "new" method if the struct doesn't already have one.  Add it
        // even if the struct implements a trait that has a new method.  The
        // trait's new method can still be called as `<MockX as TraitY>::new`
        if !has_new {
            quote!(
                /// Create a new mock object with no expectations.
                ///
                /// This method will not be generated if the real struct
                /// already has a `new` method.  However, it *will* be
                /// generated if the struct implements a trait with a `new`
                /// method.  The trait's `new` method can still be called
                /// like `<MockX as TraitY>::new`
                pub fn new() -> Self {
                    Self::default()
                }
            ).to_tokens(&mut mock_body);
        }
        // generate methods on traits
        let (ig, tg, wc) = self.generics.split_for_impl();
        quote!(impl #ig #mock_struct_name #tg #wc {#mock_body})
            .to_tokens(&mut output);
        for trait_ in self.traits.iter() {
            mock_trait_methods(&self.name, &self.generics, &trait_, &self.vis)
                .to_tokens(&mut output);
        }
        output
    }
}

impl From<ItemImpl> for ManualMock {
    fn from(item_impl: ItemImpl) -> ManualMock {
        let name = match *item_impl.self_ty {
            Type::Path(type_path) => {
                find_ident_from_path(&type_path.path).0
            },
            x => {
                compile_error(x.span(),
                    "mockall_derive only supports mocking traits and structs");
                Ident::new("", Span::call_site())
            }
        };
        let mut methods = Vec::new();
        let mut titys = Vec::new();
        let mut attrs = Attrs::default();
        for item in item_impl.items.iter() {
            match item {
                ImplItem::Const(_) => {
                    // const items can easily be added by the user in a separate
                    // impl block
                },
                ImplItem::Method(meth) => {
                    methods.push(meth.clone());
                },
                ImplItem::Type(ty) => {
                    let tity = TraitItemType {
                        attrs: ty.attrs.clone(),
                        type_token: ty.type_token,
                        ident: ty.ident.clone(),
                        generics: ty.generics.clone(),
                        colon_token: None,
                        bounds: Punctuated::new(),
                        default: Some((ty.eq_token, ty.ty.clone())),
                        semi_token: ty.semi_token
                    };
                    attrs.attrs.insert(ty.ident.clone(), ty.ty.clone());
                    titys.push(tity);
                },
                _ => {
                    compile_error(item.span(),
                    "This impl item is not yet supported by MockAll");
                }
            }
        };
        // automock makes everything public
        let pub_token = Token![pub](Span::call_site());
        let vis = Visibility::Public(VisPublic{pub_token});
        let (methods, traits) = if let Some((_, path, _)) = item_impl.trait_ {
            let mut items = Vec::new();
            for ty in titys.into_iter() {
                items.push(TraitItem::Type(ty));
            }
            for meth in methods.into_iter() {
                let tim = TraitItemMethod {
                    attrs: Vec::new(),
                    default: None,
                    sig: meth.sig.clone(),
                    semi_token: Some(Token![;](Span::call_site()))
                };
                items.push(TraitItem::Method(tim));
            }
            let path_args = &path.segments.last().unwrap().arguments;
            let trait_ = ItemTrait {
                attrs: item_impl.attrs.clone(),
                vis: vis.clone(),
                unsafety: item_impl.unsafety,
                auto_token: None,
                trait_token: token::Trait::default(),
                ident: find_ident_from_path(&path).0,
                generics: filter_generics(&item_impl.generics, path_args),
                colon_token: None,
                supertraits: Punctuated::new(),
                brace_token: token::Brace::default(),
                items
            };
            let concretized_trait = attrs.substitute_trait(&trait_);
            (Vec::new(), vec![concretized_trait])
        } else {
            assert!(titys.is_empty());
            (methods, Vec::new())
        };
        ManualMock {
            attrs: item_impl.attrs.clone(),
            vis,
            name,
            generics: item_impl.generics.clone(),
            methods,
            traits
        }
    }
}

impl From<(Attrs, ItemTrait)> for ManualMock {
    fn from((attrs, item_trait): (Attrs, ItemTrait)) -> ManualMock {
        let trait_ = attrs.substitute_trait(&item_trait);
        ManualMock {
            attrs: item_trait.attrs.clone(),
            vis: item_trait.vis.clone(),
            name: item_trait.ident.clone(),
            generics: item_trait.generics,
            methods: Vec::new(),
            traits: vec![trait_]
        }
    }
}

impl Parse for ManualMock {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let attrs = input.call(syn::Attribute::parse_outer)?;
        let vis: syn::Visibility = input.parse()?;
        let name: syn::Ident = input.parse()?;
        let mut generics: syn::Generics = input.parse()?;
        let wc: Option<syn::WhereClause> = input.parse()?;
        generics.where_clause = wc;

        let impl_content;
        let _brace_token = braced!(impl_content in input);
        let mut methods = Vec::new();
        while !impl_content.is_empty() {
            let method: syn::TraitItem = impl_content.parse()?;
            match &method {
                syn::TraitItem::Method(meth) => {
                    methods.push(tim2iim(meth, &vis))
                },
                _ => {
                    return Err(input.error("Unsupported in this context"));
                }
            }
        }

        let mut traits = Vec::new();
        while !input.is_empty() {
            let trait_: syn::ItemTrait = input.parse()?;
            traits.push(trait_);
        }

        Ok(ManualMock{attrs, vis, name, generics, methods, traits})
    }
}

/// Filter a generics list, keeping only the elements specified by path_args
/// e.g. filter_generics(<A: Copy, B: Clone>, <A>) -> <A: Copy>
fn filter_generics(g: &Generics, path_args: &PathArguments)
    -> Generics
{
    let mut params = Punctuated::new();
    match path_args {
        PathArguments::None => ()/* No generics selected */,
        PathArguments::Parenthesized(p) => {
            compile_error(p.span(),
                          "Mockall does not support mocking Fn objects");
        },
        PathArguments::AngleBracketed(abga) => {
            let args = &abga.args;
            if g.where_clause.is_some() {
                compile_error(g.where_clause.span(),
                    "Mockall does not yet support where clauses here");
                return g.clone();
            }
            for param in g.params.iter() {
                match param {
                    GenericParam::Type(tp) => {
                        let matches = |ga: &GenericArgument| {
                            if let GenericArgument::Type(
                                Type::Path(type_path)) = ga
                            {
                                type_path.path.is_ident(&tp.ident)
                            } else {
                                false
                            }
                        };
                        if args.iter().any(matches) {
                            params.push(param.clone())
                        }
                    },
                    GenericParam::Lifetime(ld) => {
                        let matches = |ga: &GenericArgument| {
                            if let GenericArgument::Lifetime(lt) = ga {
                                *lt == ld.lifetime
                            } else {
                                false
                            }
                        };
                        if args.iter().any(matches) {
                            params.push(param.clone())
                        }
                    },
                    GenericParam::Const(_) => ()/* Ignore */,
                }
            }
        }
    };
    if params.is_empty() {
        Generics::default()
    } else {
        Generics {
            lt_token: Some(Token![<](g.span())),
            params,
            gt_token: Some(Token![>](g.span())),
            where_clause: None
        }
    }
}

fn find_ident_from_path(path: &Path) -> (Ident, PathArguments) {
        if path.segments.len() != 1 {
            compile_error(path.span(),
                "mockall_derive only supports structs defined in the current module");
            return (Ident::new("", path.span()), PathArguments::None);
        }
        let last_seg = path.segments.last().unwrap();
        (last_seg.ident.clone(), last_seg.arguments.clone())
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

/// Generate a mock method and its expectation method
///
/// # Arguments
///
/// * `mock_struct_name: Name of the mock structure, not the original one, like
///                     "MockFoo".
/// * `mod_ident`:      Name of the module that contains the expectation! macro,
///                     like __mock_Foo
/// * `meth_attrs`:     Any attributes on the original method, like #[inline]
/// * `meth_vis`:       Visibility of the original method
/// * `expect_vis`:     Desired visiblity of the expect_XXX method
/// * `sig`:            Signature of the original method
/// * `sub`:            Name of the trait containing the method's expectation
///                     object, if any.
/// * `generics`:       Generics of the method's parent trait or structure,
///                     _not_ the method itself.
fn gen_mock_method(mock_struct_name: &syn::Ident,
                   mod_ident: Option<&syn::Ident>,
                   meth_attrs: &[syn::Attribute],
                   meth_vis: &syn::Visibility,
                   expect_vis: &syn::Visibility,
                   sig: &syn::Signature,
                   sub: Option<&syn::Ident>,
                   generics: &syn::Generics)
    -> (TokenStream, TokenStream, TokenStream)
{
    assert!(sig.variadic.is_none(),
        "MockAll does not yet support variadic functions");
    let mut mock_output = TokenStream::new();
    let mut expect_output = TokenStream::new();
    let mut cp_output = TokenStream::new();
    let constness = sig.constness;
    let unsafety = sig.unsafety;
    let asyncness = sig.asyncness;
    let abi = &sig.abi;
    let fn_token = &sig.fn_token;
    let ident = &sig.ident;
    let private_meth_ident = format_ident!("__{}", &ident);
    let mut meth_types = method_types(sig, Some(generics));
    let merged_g = merge_generics(&generics, &meth_types.expectation_generics);
    let inputs = &meth_types.inputs;
    let output = &mut meth_types.output;
    if let ReturnType::Type(_, ty) = output {
        deselfify(ty, mock_struct_name, generics);
    }
    let attrs_with_docs = format_attrs(meth_attrs, true);
    let attrs_nodocs = format_attrs(meth_attrs, false);

    // First the mock method
    {
        let (ig, _, wc) = sig.generics.split_for_impl();
        quote!(#attrs_with_docs #meth_vis #constness #unsafety #asyncness #abi
               #fn_token #ident #ig (#inputs) #output #wc)
            .to_tokens(&mut mock_output);
    }

    let expectation = &meth_types.expectation;
    let call = &meth_types.call;
    let call_exprs = &meth_types.call_exprs;
    let mut args = Vec::new();
    let expect_obj_name = if let Some(s) = sub {
        let sub_struct = format_ident!("{}_expectations", s);
        quote!(self.#sub_struct.#ident)
    } else {
        quote!(self.#ident)
    };
    for p in inputs.iter() {
        match p {
            syn::FnArg::Receiver(_) => {
                // Don't output the "self" argument
            },
            syn::FnArg::Typed(arg) => {
                args.push(arg.pat.clone());
            },
        }
    }

    let (ig, _, wc) = meth_types.expectation_generics.split_for_impl();
    let tbf_g = if meth_types.is_static || meth_types.is_expectation_generic {
        // For generic and static methods only, the trait's generic parameters
        // become generic parameters of the method.
        &merged_g
    } else {
        &meth_types.expectation_generics
    }.clone();
    let (tbf_tg, _, _) = split_lifetimes(tbf_g, &inputs, &sig.output);
    let (_, tg, _) = tbf_tg.split_for_impl();
    let call_turbofish = tg.as_turbofish();
    let no_match_msg = format!("{}::{}: No matching expectation found",
        mock_struct_name, ident);
    if meth_types.is_static {
        quote!({
            {
                let __mockall_guard = #mod_ident::#private_meth_ident::EXPECTATIONS
                    .lock().unwrap();
                /*
                 * TODO: catch panics, then gracefully release the mutex so it
                 * won't be poisoned.  This requires bounding any generic
                 * parameters with UnwindSafe
                 */
                /* std::panic::catch_unwind(|| */
                __mockall_guard.#call#call_turbofish(#call_exprs)
                /*)*/
            }.expect(#no_match_msg)
            /*}.unwrap()*/
        })
    } else {
        quote!({
            #expect_obj_name.#call#call_turbofish(#call_exprs)
            .expect(#no_match_msg)
        })
    }.to_tokens(&mut mock_output);

    // Then the expectation method
    if meth_types.is_static {
        let context_ident = format_ident!("{}_context", ident);
        let context_generics = strip_generics_lifetimes(generics);
        let (_, ctx_tg, _) = context_generics.split_for_impl();
        let context_docstr = {
            let inner_ds = format!("Create a [`Context`]({}/struct.Context.html) for mocking the `{}` method",
            // I don't think the unwrap should ever fail, but if it does it's
            // better than we screw up the doc comment than panic.  Hence "XXX"
                mod_ident.unwrap_or(&Ident::new("XXX", Span::call_site())),
                ident);
            quote!( #[doc = #inner_ds])
        };
        quote!(#attrs_nodocs
               #context_docstr
               #expect_vis fn #context_ident()
               -> #mod_ident::#private_meth_ident::Context #ctx_tg
            {
                #mod_ident::#private_meth_ident::Context::default()
            }
        )
    } else {
        let expect_ident = format_ident!("expect_{}", ident);

        #[cfg(not(feature = "nightly_derive"))]
        let must_use = quote!(#[must_use =
                "Must set return value when not using the \"nightly\" feature"
            ]);
        #[cfg(feature = "nightly_derive")]
        let must_use = quote!();

        quote!(
            #must_use
            #attrs_nodocs
            /// Create an
            /// [`Expectation`](#mod_ident/ident/struct.Expectation.html) for
            /// mocking the `ident` method
            #expect_vis fn #expect_ident #ig(&mut self)
               -> &mut #mod_ident::#expectation
               #wc
            {
                #expect_obj_name.expect#call_turbofish()
            }
        )
    }.to_tokens(&mut expect_output);

    // Finally this method's contribution to the checkpoint method
    if meth_types.is_static {
        // Don't checkpoint static methods.  They get checkpointed by their
        // context objects instead.
        quote!()
    } else {
        quote!(#attrs_nodocs { #expect_obj_name.checkpoint(); })
    }.to_tokens(&mut cp_output);

    (mock_output, expect_output, cp_output)
}

fn gen_struct<T>(attrs: &[syn::Attribute],
                 mock_ident: &syn::Ident,
                 vis: &syn::Visibility,
                 ident: &syn::Ident,
                 generics: &syn::Generics,
                 subs: &[(String, syn::Generics)],
                 methods: &[T]) -> TokenStream
    where T: Borrow<syn::ImplItemMethod>
{
    let mut output = TokenStream::new();
    let mod_ident = gen_mod_ident(&ident, None);
    let ident = gen_mock_ident(&ident);
    let mut body = TokenStream::new();
    let mut mod_body = TokenStream::new();
    let mut default_body = TokenStream::new();

    let mut attr_ts = TokenStream::new();
    for attr in attrs{
        attr.to_tokens(&mut attr_ts);
    }

    // Make Expectation fields for each method
    for (sub, sub_generics) in subs.iter() {
        let (_, tg, _) = sub_generics.split_for_impl();
        let sub_struct = format_ident!("{}_expectations", sub);
        let sub_mock = format_ident!("{}_{}", ident, sub);
        quote!(#sub_struct: #sub_mock #tg,).to_tokens(&mut body);
        quote!(#sub_struct: #sub_mock::default(),)
            .to_tokens(&mut default_body)
    }
    for meth in methods.iter() {
        let attrs = format_attrs(&meth.borrow().attrs, false);
        let method_ident = &meth.borrow().sig.ident;
        let meth_types = method_types(&meth.borrow().sig, Some(generics));
        let expect_obj = &meth_types.expect_obj;
        let expectations = &meth_types.expectations;
        let meth_ident = &meth.borrow().sig.ident;
        let private_meth_ident = format_ident!("__{}", &meth_ident);
        let output = &meth_types.output;

        let expect_vis = expectation_visibility(&meth.borrow().vis, 2);

        Expectation::new(&attrs, &meth_types.expectation_inputs,
                         Some(&generics), &meth_types.expectation_generics,
                         meth_ident, &private_meth_ident, Some(&mock_ident),
                         output,
                         &expect_vis, 2).to_tokens(&mut mod_body);

        if !meth_types.is_static {
            quote!(#attrs #method_ident: #mod_ident::#expect_obj,)
                .to_tokens(&mut body);
            quote!(#attrs #method_ident: #mod_ident::#expectations::default(),)
                .to_tokens(&mut default_body)
        }
    }

    // Make PhantomData fields, if necessary
    for (count, param) in generics.params.iter().enumerate() {
        let phident = format_ident!("_t{}", count);
        match param {
            syn::GenericParam::Lifetime(l) => {
                if !l.bounds.is_empty() {
                    compile_error(l.bounds.span(),
                        "#automock does not yet support lifetime bounds on structs");
                }
                let lifetime = &l.lifetime;
                quote!(#phident: ::std::marker::PhantomData<&#lifetime ()>,)
                    .to_tokens(&mut body);
            },
            syn::GenericParam::Type(tp) => {
                let ty = &tp.ident;
                quote!(#phident: ::std::marker::PhantomData<#ty>,)
                    .to_tokens(&mut body);
            },
            syn::GenericParam::Const(_) => {
                compile_error(param.span(),
                    "#automock does not yet support generic constants");
            }
        }
        quote!(#phident: ::std::marker::PhantomData,)
            .to_tokens(&mut default_body);
    }
    let (ig, tg, wc) = generics.split_for_impl();
    quote!(
        #[allow(non_snake_case)]
        #[doc(hidden)]
        pub mod #mod_ident {
            use super::*;
            #mod_body
        }
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
        #[allow(missing_docs)]
        #attr_ts
        #vis struct #ident #ig #wc {
            #body
        }
    ).to_tokens(&mut output);
    quote!(impl #ig ::std::default::Default for #ident #tg #wc {
        fn default() -> Self {
            Self {
                #default_body
            }
        }
    }).to_tokens(&mut output);

    output
}

/// Generate mock methods for a Trait
///
/// # Parameters
///
/// * `struct_ident`:       Name of the structure to mock
/// * `struct_generics`:    If provided, use these generic fields for the
///                         Mock struct.  Otherwise, generate the struct's
///                         generics from the Trait
/// * `item`:               The trait whose methods are being mocked
/// * `vis`:                Visibility of the struct
fn mock_trait_methods(struct_ident: &syn::Ident,
                      struct_generics: &syn::Generics,
                      item: &syn::ItemTrait,
                      vis: &syn::Visibility) -> TokenStream
{
    let mut output = TokenStream::new();
    let mut mock_body = TokenStream::new();
    let mut expect_body = TokenStream::new();
    let mock_ident = gen_mock_ident(&struct_ident);

    for trait_item in item.items.iter() {
        match trait_item {
            syn::TraitItem::Const(_) => {
                // Nothing to implement
            },
            syn::TraitItem::Method(meth) => {
                let mod_ident = gen_mod_ident(&struct_ident, Some(&item.ident));
                let generics = merge_generics(&struct_generics, &item.generics);
                let (mock_meth, expect_meth, _cp) = gen_mock_method(
                    &mock_ident,
                    Some(&mod_ident),
                    &meth.attrs[..],
                    &syn::Visibility::Inherited,
                    vis,
                    &meth.sig,
                    Some(&item.ident),
                    &generics
                );
                // trait methods must have inherited visibility.  Expectation
                // methods should have public, for lack of any clearer option.
                mock_meth.to_tokens(&mut mock_body);
                expect_meth.to_tokens(&mut expect_body);
            },
            syn::TraitItem::Type(ty) => {
                if !ty.generics.params.is_empty() {
                    compile_error(ty.generics.span(),
                        "Mockall does not yet support generic associated types");
                }
                if ty.default.is_some() {
                    // Trait normally can't get here (unless the
                    // associated_type_defaults feature is enabled), but we can
                    // get here from mock! if invoked like
                    // mock!{
                    //     Foo { }
                    //     trait Bar {
                    //         type A=B;
                    //     }
                    // }
                    ty.to_tokens(&mut mock_body)
                } else {
                    compile_error(ty.span(), "Associated types must be made concrete for mocking.");
                }
            },
            _ => {
                compile_error(trait_item.span(),
                    "This impl item is not yet supported by MockAll");
            }
        }
    }

    // Put all mock methods in one impl block
    item.unsafety.to_tokens(&mut output);
    let ident = &item.ident;
    let (s_ig, s_sg, s_wc) = struct_generics.split_for_impl();
    let (_t_ig, t_tg, _t_wc) = item.generics.split_for_impl();
    quote!(impl #s_ig #ident #t_tg
           for #mock_ident #s_sg #s_wc {
        #mock_body
    }).to_tokens(&mut output);

    // Put all expect methods in a separate impl block.  This is necessary when
    // mocking a trait impl, where we can't add any new methods
    quote!(impl #s_ig #mock_ident #s_sg #s_wc {
        #expect_body
    }).to_tokens(&mut output);

    output
}

fn tim2iim(m: &syn::TraitItemMethod, vis: &syn::Visibility)
    -> syn::ImplItemMethod
{
    syn::ImplItemMethod{
        attrs: m.attrs.clone(),
        vis: vis.clone(),
        defaultness: None,
        sig: m.sig.clone(),
        block: syn::parse2(quote!({})).unwrap(),
    }
}

/// Test cases for `mock!{}`.
#[cfg(test)]
mod t {

    use std::str::FromStr;
    use super::super::*;

    /// Mocking a private struct.
    #[test]
    fn priv_struct() {
        let code = r#"
            Foo {
                fn foo(&self, x: u32) -> i64;
            }
        "#;
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let output = do_mock(ts).to_string();
        assert!(!output.contains("pub struct MockFoo"));
    }

    #[test]
    fn doc_comments() {
        let code = r#"
            /// Struct docs
            pub Foo {
                /// Method docs
                fn foo(&self);
            }
            trait Tr {
                /// Trait method docs
                fn bar(&self);
            }
        "#;
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let output = do_mock(ts)
            .to_string()
            // Strip spaces so we don't get test regressions due to minor
            // formatting changes
            .replace(" ", "");
        assert!(output.contains(r#"#[doc="Methoddocs"]pubfnfoo"#));
        assert!(output.contains(r#"#[doc="Structdocs"]pubstructMockFoo"#));
        assert!(output.contains(r#"#[doc="Traitmethoddocs"]fnbar"#));
    }

}
