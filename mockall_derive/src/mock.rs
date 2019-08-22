// vim: tw=80
use super::*;
use quote::ToTokens;
use std::{
    borrow::Borrow,
    env
};
use syn::parse::{Parse, ParseStream};

pub(crate) struct Mock {
    pub(crate) vis: syn::Visibility,
    pub(crate) name: syn::Ident,
    pub(crate) generics: syn::Generics,
    // The Mock struct's inherent methods.  The blocks will all be empty.
    pub(crate) methods: Vec<syn::ImplItemMethod>,
    pub(crate) traits: Vec<syn::ItemTrait>
}

impl Mock {
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
        gen_struct(&mock_struct_name, &self.vis, &self.name, &self.generics,
                   &subs, &self.methods)
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
            gen_struct(&mock_struct_name, &vis, &sub_mock, &self.generics, &[],
                       &methods)
                .to_tokens(&mut output);
            let mock_sub_name = gen_mock_ident(&sub_mock);
            for meth in methods {
                has_new |= meth.borrow().sig.ident == "new";
                let generics = merge_generics(&self.generics, &trait_.generics);
                let (_, _, cp) = gen_mock_method(Some(&mod_ident),
                                                 &meth.attrs[..],
                                                 &meth.vis, &meth.vis,
                                                 &meth.borrow().sig, None,
                                                 &generics);
                cp.to_tokens(&mut sub_cp_body);
            }
            let (ig, tg, wc) = self.generics.split_for_impl();
            quote!(impl #ig #mock_sub_name #tg #wc {
                fn checkpoint(&mut self) {
                    #sub_cp_body
                }
            }).to_tokens(&mut output);
            quote!(self.#sub_struct.checkpoint();).to_tokens(&mut cp_body);
        }
        // generate methods on the mock structure itself
        for meth in self.methods.iter() {
            has_new |= meth.sig.ident == "new";
            let (mm, em, cp) = gen_mock_method(Some(&mock_mod_ident),
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
        #[cfg(all(not(test),feature = "extra-docs"))]
        let checkpoint_docs = quote!(
            #[doc = "Immediately validate all expectations and clear them."]
        );
        #[cfg(any(test, not(feature = "extra-docs")))]
        let checkpoint_docs: Option<syn::Attribute> = None;
        quote!(
            #checkpoint_docs
            pub fn checkpoint(&mut self) {
                #cp_body
            }
        ).to_tokens(&mut mock_body);
        // Add a "new" method if the struct doesn't already have one.  Add it
        // even if the struct implements a trait that has a new method.  The
        // trait's new method can still be called as `<MockX as TraitY>::new`
        if !has_new {
            #[cfg(all(not(test),feature = "extra-docs"))]
            let docstr = {
                let inner_ds = concat!(
                    "Create a new mock object with no expectations.\n\n",
                    "This method will not be generated if the real struct ",
                    "already has a `new` method.  However, it *will* be ",
                    "generated if the struct implements a trait with a `new` ",
                    "method.  The trait's `new` method can still be called ",
                    "like `<MockX as TraitY>::new`");
                quote!( #[doc = #inner_ds])
            };
            #[cfg(any(test, not(feature = "extra-docs")))]
            let docstr: Option<syn::Attribute> = None;
            quote!(
                #docstr
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

impl Parse for Mock {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
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

        Ok(Mock{vis, name, generics, methods, traits})
    }
}

fn format_attrs(attrs: &[syn::Attribute]) -> TokenStream {
    let mut out = TokenStream::new();
    for attr in attrs {
        if attr.path.get_ident().map(|i| i == "doc").unwrap_or(false) {
            // Discard doc attributes from the mock object.  They cause a bunch
            // of warnings.
            continue;
        }
        attr.to_tokens(&mut out);
    }
    out
}

/// Generate a mock method and its expectation method
///
/// # Arguments
///
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
fn gen_mock_method(mod_ident: Option<&syn::Ident>,
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
    let meth_types = method_types(sig, Some(generics));
    let merged_g = merge_generics(&generics, &meth_types.expectation_generics);
    let inputs = &meth_types.inputs;
    let output = &meth_types.output;
    let attrs = format_attrs(meth_attrs);

    // First the mock method
    {
        let (ig, _, wc) = sig.generics.split_for_impl();
        quote!(#attrs #meth_vis #constness #unsafety #asyncness #abi
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

    let (ig, ex_tg, wc) = meth_types.expectation_generics.split_for_impl();
    let tg = if meth_types.is_static || meth_types.is_expectation_generic {
        // For generic and static methods only, the trait's generic parameters
        // become generic parameters of the method.
        merged_g.split_for_impl().1
    } else {
        ex_tg
    };
    let call_turbofish = tg.as_turbofish();
    if meth_types.is_static {
        quote!({
            {
                let __mockall_guard = #mod_ident::#ident::EXPECTATIONS
                    .lock().unwrap();
                /*
                 * TODO: catch panics, then gracefully release the mutex so it
                 * won't be poisoned.  This requires bounding any generic
                 * parameters with UnwindSafe
                 */
                /* std::panic::catch_unwind(|| */
                __mockall_guard.#call #call_turbofish(#call_exprs)
                /*)*/
            }.expect("No matching expectation found")
            /*}.unwrap()*/
        })
    } else {
        quote!({
            #expect_obj_name.#call#call_turbofish(#call_exprs)
            .expect("No matching expectation found")
        })
    }.to_tokens(&mut mock_output);

    // Then the expectation method
    if meth_types.is_static {
        #[cfg(all(not(test),feature = "extra-docs"))]
        let docstr = {
            let inner_ds = format!("Create a [`Context`]({}/{}/struct.Context.html) for mocking the `{}` method",
                quote!(#mod_ident), ident, ident);
            quote!( #[doc = #inner_ds])
        };
        #[cfg(any(test, not(feature = "extra-docs")))]
        let docstr: Option<syn::Attribute> = None;
        let context_ident = format_ident!("{}_context", ident);
        quote!(#attrs #docstr #expect_vis fn #context_ident()
               -> #mod_ident::#ident::Context
            {
                #mod_ident::#ident::Context{}
            }
        )
    } else {
        #[cfg(all(not(test),feature = "extra-docs"))]
        let docstr = {
            let inner_ds = format!("Create an [`Expectation`]({}/{}/struct.Expectation.html) for mocking the `{}` method",
                quote!(#mod_ident), ident, ident);
            quote!( #[doc = #inner_ds])
        };
        #[cfg(any(test, not(feature = "extra-docs")))]
        let docstr: Option<syn::Attribute> = None;
        let expect_ident = format_ident!("expect_{}", ident);
        quote!(#attrs #docstr #expect_vis fn #expect_ident #ig(&mut self)
               -> &mut #mod_ident::#expectation
               #wc
        {
            #expect_obj_name.expect#call_turbofish()
        })
    }.to_tokens(&mut expect_output);

    // Finally this method's contribution to the checkpoint method
    if meth_types.is_static {
        quote!(#attrs {
            let _timeses = #mod_ident::#ident::EXPECTATIONS.lock().unwrap()
                .checkpoint()
                .collect::<Vec<_>>();
        })
    } else {
        quote!(#attrs { #expect_obj_name.checkpoint(); })
    }.to_tokens(&mut cp_output);

    (mock_output, expect_output, cp_output)
}

fn gen_struct<T>(mock_ident: &syn::Ident,
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
        let attrs = format_attrs(&meth.borrow().attrs);
        let method_ident = &meth.borrow().sig.ident;
        let meth_types = method_types(&meth.borrow().sig, Some(generics));
        let expect_obj = &meth_types.expect_obj;
        let expectations = &meth_types.expectations;
        let meth_ident = &meth.borrow().sig.ident;
        let output = &meth_types.output;

        let expect_vis = expectation_visibility(&meth.borrow().vis, 2);
        let mut macro_g = TokenStream::new();
        let merged_g = merge_generics(&generics, &meth_types.expectation_generics);
        if ! merged_g.params.is_empty() {
            merged_g.split_for_impl().1.to_tokens(&mut macro_g)
        } else {
            // expectation! requires the <> even if it's empty
            quote!(<>).to_tokens(&mut macro_g);
        }

        Expectation::new(&attrs, &meth_types.expectation_inputs,
                         Some(&generics), &meth_types.expectation_generics,
                         meth_ident, meth_ident, Some(&mock_ident), output,
                         &expect_vis).to_tokens(&mut mod_body);

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
        pub mod #mod_ident {
            use super::*;
            #mod_body
        }
        #[allow(non_camel_case_types)]
        #[allow(non_snake_case)]
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

pub(crate) fn do_mock(input: TokenStream) -> TokenStream {
    let mock: Mock = match syn::parse2(input) {
        Ok(mock) => mock,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    if env::var("MOCKALL_DEBUG").is_ok() {
        println!("{}", mock.gen());
    }
    mock.gen()
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

}
