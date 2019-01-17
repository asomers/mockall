// vim: tw=80
#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Token
};

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

struct Mock {
    vis: syn::Visibility,
    name: syn::Ident,
    generics: syn::Generics,
    methods: Vec<syn::TraitItemMethod>,
    traits: Vec<syn::ItemTrait>
}

impl Mock {
    fn gen(&self) -> TokenStream {
        let mut output = TokenStream::new();
        let mut mock_body = TokenStream::new();
        let mock_struct_name = gen_mock_ident(&self.name);
        gen_struct(&self.vis, &self.name, &self.generics)
            .to_tokens(&mut output);
        for meth in self.methods.iter() {
            // All mocked methods are public
            let pub_token = syn::token::Pub{span: Span::call_site()};
            let vis = syn::Visibility::Public(syn::VisPublic{pub_token});
            let (mm, em) = gen_mock_method(None, &vis, &meth.sig);
            mm.to_tokens(&mut mock_body);
            em.to_tokens(&mut mock_body);
        }
        let generics = &self.generics;
        let reduced_generics = reduce_generics(&generics);
        quote!(impl #generics #mock_struct_name #reduced_generics {#mock_body})
            .to_tokens(&mut output);
        for trait_ in self.traits.iter() {
            mock_trait_methods(&mock_struct_name, Some(&generics), &trait_)
                .to_tokens(&mut output);
        }
        output
    }
}

impl Parse for Mock {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let vis: syn::Visibility = input.parse()?;
        let name: syn::Ident = input.parse()?;
        let generics: syn::Generics = input.parse()?;

        let impl_content;
        let _brace_token = braced!(impl_content in input);
        let methods_item: syn::punctuated::Punctuated<syn::TraitItem, Token![;]>
            = impl_content.parse_terminated(syn::TraitItem::parse)?;
        let mut methods = Vec::new();
        for method in methods_item.iter() {
            match method {
                syn::TraitItem::Method(meth) => methods.push(meth.clone()),
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

/// Generate a mock identifier from the regular one: eg "Foo" => "MockFoo"
fn gen_mock_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("Mock{}", ident), ident.span())
}

/// Remove the bounds from  a Generics.  Eg:
/// reduce_generics(<'a, T: Copy>) == Ok(<'a, T>)
fn reduce_generics(g: &syn::Generics) -> syn::Generics {
    let mut params = syn::punctuated::Punctuated::new();
    for param in g.params.iter() {
        match param {
            syn::GenericParam::Type(ty) => {
                let mut newty = ty.clone();
                newty.colon_token = None;
                newty.bounds = syn::punctuated::Punctuated::new();
                newty.eq_token = None;
                newty.default = None;
                params.push(syn::GenericParam::Type(newty));
            },
            syn::GenericParam::Lifetime(lt) => {
                let mut newlt = lt.clone();
                newlt.colon_token = None;
                newlt.bounds = syn::punctuated::Punctuated::new();
                params.push(syn::GenericParam::Lifetime(newlt));
            },
            syn::GenericParam::Const(_) => {
                // https://github.com/rust-lang/rust/issues/44580
                compile_error(param.span(),
                    "Generic constants are not yet supported");
            }
        }
    }
    syn::Generics {
        lt_token: g.lt_token,
        params,
        gt_token: g.gt_token,
        where_clause: None
    }
}

/// Generate a mock path from a regular one:
/// eg "foo::bar::Baz" => "foo::bar::MockBaz"
fn gen_mock_path(path: &syn::Path) -> syn::Path {
    let mut outsegs = path.segments.clone();
    let mut last_seg = outsegs.last_mut().unwrap();
    last_seg.value_mut().ident = gen_mock_ident(&last_seg.value().ident);
    syn::Path{leading_colon: path.leading_colon, segments: outsegs}
}

/// Generate a mock method and its expectation method
fn gen_mock_method(defaultness: Option<&syn::token::Default>,
                   vis: &syn::Visibility,
                   sig: &syn::MethodSig) -> (TokenStream, TokenStream)
{
    assert!(sig.decl.variadic.is_none(),
        "MockAll does not yet support variadic functions");
    let mut mock_output = TokenStream::new();
    let mut expect_output = TokenStream::new();
    let constness = sig.constness;
    let unsafety = sig.unsafety;
    let asyncness = sig.asyncness;
    let abi = &sig.abi;
    let fn_token = &sig.decl.fn_token;
    let ident = &sig.ident;
    let generics = &sig.decl.generics;
    let inputs = &sig.decl.inputs;
    let output = &sig.decl.output;

    // First the mock method
    quote!(#defaultness #vis #constness #unsafety #asyncness #abi
           #fn_token #ident #generics (#inputs) #output)
        .to_tokens(&mut mock_output);

    let mut is_static = true;
    let mut input_type
        = syn::punctuated::Punctuated::<syn::Type, Token![,]>::new();
    for fn_arg in sig.decl.inputs.iter() {
        match fn_arg {
            syn::FnArg::Captured(arg) => input_type.push(arg.ty.clone()),
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
    let output_type: syn::Type = match &sig.decl.output {
        syn::ReturnType::Default => {
            let paren_token = syn::token::Paren{span: Span::call_site()};
            let elems = syn::punctuated::Punctuated::new();
            syn::Type::Tuple(syn::TypeTuple{paren_token, elems})
        },
        syn::ReturnType::Type(_, ty) => (**ty).clone()
    };
    if is_static {
        quote!({unimplemented!("Expectations on static methods are TODO");})
            .to_tokens(&mut mock_output);
        return (mock_output, TokenStream::new())
    }
    let ident = format!("{}", sig.ident);
    let mut args = Vec::new();
    for p in sig.decl.inputs.iter() {
        match p {
            syn::FnArg::SelfRef(_) | syn::FnArg::SelfValue(_) => {
                // Don't output the "self" argument
            },
            syn::FnArg::Captured(arg) => {
                let pat = &arg.pat;
                args.push(quote!(#pat));
            },
            _ => compile_error(p.span(),
                "Should be unreachable for normal Rust code")
        }
    }

    quote!({self.e.called::<(#input_type), #output_type>(#ident, (#(#args),*))})
        .to_tokens(&mut mock_output);

    // Then the expectation method
    let expect_ident = syn::Ident::new(&format!("expect_{}", sig.ident),
                                       sig.ident.span());
    quote!(pub fn #expect_ident #generics(&mut self)
           -> &mut ::mockall::Expectation<(#input_type), #output_type> {
        self.e.expect::<(#input_type), #output_type>(#ident)
   }).to_tokens(&mut expect_output);

    (mock_output, expect_output)
}

/// Implement a struct's methods on its mock struct
fn mock_impl(item: syn::ItemImpl) -> TokenStream {
    let mut output = TokenStream::new();
    let mut mock_body = TokenStream::new();
    let mut expect_body = TokenStream::new();

    let mock_type = match *item.self_ty {
        syn::Type::Path(type_path) => {
            assert!(type_path.qself.is_none(), "What is qself?");
            gen_mock_path(&type_path.path)
        },
        _ => {
            compile_error(item.self_ty.span(),
                "MockAll only supports implementing methods on Structs");
            return TokenStream::new();
        }
    };

    for impl_item in item.items {
        match impl_item {
            syn::ImplItem::Const(_) => {
                // const items can easily be added by the user in a separate
                // impl block
            },
            syn::ImplItem::Existential(ty) => ty.to_tokens(&mut mock_body),
            syn::ImplItem::Type(ty) => ty.to_tokens(&mut mock_body),
            syn::ImplItem::Method(meth) => {
                let (mock_meth, expect_meth) = gen_mock_method(
                    meth.defaultness.as_ref(),
                    &meth.vis,
                    &meth.sig
                );
                mock_meth.to_tokens(&mut mock_body);
                expect_meth.to_tokens(&mut expect_body);
            },
            _ => compile_error(impl_item.span(),
                "This impl item is not yet supported by MockAll")
        }
    }

    // Put all mock methods in one impl block
    item.unsafety.to_tokens(&mut output);
    item.impl_token.to_tokens(&mut output);
    item.generics.to_tokens(&mut output);
    if let Some(trait_) = item.trait_ {
        let (bang, path, for_) = trait_;
        if let Some(b) = bang {
            b.to_tokens(&mut output);
        }
        path.to_tokens(&mut output);
        for_.to_tokens(&mut output);
    }
    mock_type.to_tokens(&mut output);
    quote!({#mock_body}).to_tokens(&mut output);

    // Put all expect methods in a separate impl block.  This is necessary when
    // mocking a trait impl, where we can't add any new methods
    item.impl_token.to_tokens(&mut output);
    item.generics.to_tokens(&mut output);
    mock_type.to_tokens(&mut output);
    quote!({#expect_body}).to_tokens(&mut output);

    output
}

fn gen_struct(vis: &syn::Visibility,
              ident: &syn::Ident,
              generics: &syn::Generics) -> TokenStream
{
    let mut output = TokenStream::new();
    let ident = gen_mock_ident(&ident);
    let mut body: TokenStream = "e: ::mockall::Expectations,".parse().unwrap();
    let mut count = 0;
    for param in generics.params.iter() {
        let phname = format!("_t{}", count);
        let phident = syn::Ident::new(&phname, Span::call_site());
        match param {
            syn::GenericParam::Lifetime(l) => {
                assert!(l.bounds.is_empty(),
                    "#automock does not yet support lifetime bounds on structs");
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
        count += 1;
    }
    quote!(
        #[derive(Default)]
        #vis struct #ident #generics {
            #body
        }
    ).to_tokens(&mut output);

    output
}

fn mock_struct(item: syn::ItemStruct) -> TokenStream {
    gen_struct(&item.vis, &item.ident, &item.generics)
}

/// Merge two Generics lists into one.  The parameter names must be disjoint.
fn merge_generics(g0: &syn::Generics, g1: &syn::Generics) -> syn::Generics {
    let lt_token = g0.lt_token.or(g1.lt_token);
    let gt_token = g0.gt_token.or(g1.gt_token);
    let mut params = g0.params.clone();
    for param in g1.params.iter() {
        params.push(param.clone());
    }
    let where_clause = match (&g0.where_clause, &g1.where_clause) {
        (None, None) => None,
        (Some(wc), None) => Some(wc.clone()),
        (None, Some(wc)) => Some(wc.clone()),
        (Some(wc0), Some(wc1)) => {
            let mut wc = wc0.clone();
            for wp in wc1.predicates.iter() {
                wc.predicates.push(wp.clone());
            }
            Some(wc)
        }
    };
    syn::Generics{lt_token, params, gt_token, where_clause}
}

/// Generate mock methods for a Trait
///
/// # Parameters
///
/// * `mock_ident`:         Name of the mock structure to generate
/// * `struct_generics`:    If provided, use these generic fields for the
///                         Mock struct.  Otherwise, generate the struct's
///                         generics from the Trait
/// * `item`:               The trait whose methods are being mocked
fn mock_trait_methods(mock_ident: &syn::Ident,
                      struct_generics: Option<&syn::Generics>,
                      item: &syn::ItemTrait) -> TokenStream
{
    let mut output = TokenStream::new();
    let mut mock_body = TokenStream::new();
    let mut expect_body = TokenStream::new();

    for trait_item in item.items.iter() {
        match trait_item {
            syn::TraitItem::Const(_) => {
                // Nothing to implement
            },
            syn::TraitItem::Method(meth) => {
                let (mock_meth, expect_meth) = gen_mock_method(
                    None,
                    &syn::Visibility::Inherited,
                    &meth.sig
                );
                mock_meth.to_tokens(&mut mock_body);
                expect_meth.to_tokens(&mut expect_body);
            },
            syn::TraitItem::Type(ty) => {
                assert!(ty.generics.params.is_empty(),
                    "Mockall does not yet support generic associated types");
                assert!(ty.bounds.is_empty(),
                    "Mockall does not yet support associated types with trait bounds");
                if ty.default.is_some() {
                    // Trait normally can't get here (unless the
                    // associated_type_defaults feature is enabled), but we can
                    // get here from mock! if invoked like
                    // mock!
                    // mock!{
                    //     Foo { }
                    //     trait Bar {
                    //         type A=B;
                    //     }
                    // }
                    ty.to_tokens(&mut mock_body)
                } else {
                    compile_error(ty.span(),
                        "MockAll does not yet support mocking traits with associated types");
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
    let trait_generics = &item.generics;
    let (merged_g, reduced_struct_g) = match struct_generics {
        None => (trait_generics.clone(),
            reduce_generics(&trait_generics)),
        Some(g) => {
            (merge_generics(g, trait_generics), reduce_generics(&g))
        }
    };
    let reduced_trait_g = reduce_generics(&trait_generics);
    quote!(impl #merged_g #ident #reduced_trait_g
           for #mock_ident #reduced_struct_g {
        #mock_body
    }).to_tokens(&mut output);

    // Put all expect methods in a separate impl block.  This is necessary when
    // mocking a trait impl, where we can't add any new methods
    quote!(impl #merged_g #mock_ident #reduced_struct_g {
        #expect_body
    }).to_tokens(&mut output);

    output
}

/// Generate a mock struct that implements a trait
fn mock_trait(item: syn::ItemTrait) -> TokenStream {
    let mut output = gen_struct(&item.vis, &item.ident, &item.generics);
    let mock_ident = gen_mock_ident(&item.ident);
    mock_trait_methods(&mock_ident, None, &item).to_tokens(&mut output);
    output
}

fn mock_item(input: TokenStream) -> TokenStream {
    let item: syn::Item = match syn::parse2(input) {
        Ok(item) => item,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    match item {
        syn::Item::Struct(item_struct) => mock_struct(item_struct),
        syn::Item::Impl(item_impl) => mock_impl(item_impl),
        syn::Item::Trait(item_trait) => mock_trait(item_trait),
        _ => {
            compile_error(item.span(),
                "#[automock] does not support this item type");
            TokenStream::new()
        }
    }
}

fn do_mock(input: TokenStream) -> TokenStream {
    let mock: Mock = match syn::parse2(input) {
        Ok(mock) => mock,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    mock.gen()
}

/// Manually mock a structure.
///
/// Sometimes `automock` can't be used.  In those cases you can use `mock!`,
/// which basically involves repeat the struct's or trait's definitions.
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
/// ```
/// # use mockall_derive::mock;
/// trait Foo {
///     fn foo(&self, x: u32);
/// }
/// mock!{
///     pub MyStruct<T: Clone> {
///         fn bar(&self) -> u8;
///     }
///     trait Foo {
///         fn foo(&self, x: u32);
///     }
/// }
/// # fn main() {}
/// ```
#[proc_macro]
pub fn mock(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_mock(item.into()).into()
}

/// Automatically generate mock types for Structs and Traits.
#[proc_macro_attribute]
pub fn automock(_attr: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let input: proc_macro2::TokenStream = input.into();
    let mut output = input.clone();
    output.extend(mock_item(input));
    output.into()
}

/// Test cases for `#[automock]`.
#[cfg(test)]
mod t {

use pretty_assertions::assert_eq;
use std::str::FromStr;
use super::*;

fn check<F: Fn(TokenStream) -> TokenStream>(desired: &str, code: &str, f: F) {
    let ts = proc_macro2::TokenStream::from_str(code).unwrap();
    let output = f(ts).to_string();
    // Let proc_macro2 reformat the whitespace in the expected string
    let expected = proc_macro2::TokenStream::from_str(desired).unwrap()
        .to_string();
    assert_eq!(expected, output);
}

#[test]
#[ignore("automocking associated types are TODO")]
fn associated_types() {
    check(r#"
    #[derive(Default)]
    struct MockA {
        e: ::mockall::Expectations,
    }
    impl A for MockA {
        type T = u32;
        fn foo(&self, x: Self::T) -> Self::T {
            self.e.called:: <(Self::T), Self::T>("foo", (x))
        }
    }
    impl MockA {
        pub fn expect_foo(&mut self)
            -> &mut ::mockall::Expectation<(<Self as A>::T), i64>
        {
            self.e.expect:: <(<Self as A>::T), i64>("foo")
        }
    }"#, r#"
    trait A {
        type T;
        fn foo(&self, x: Self::T) -> Self::T;
    }"#,
    mock_item);
}

/// Mocking a struct that's defined in another crate
#[test]
fn external_struct() {
    let desired = r#"
        #[derive(Default)]
        struct MockExternalStruct {
            e: ::mockall::Expectations,
        }
        impl MockExternalStruct {
            pub fn foo(&self, x: u32) -> i64 {
                self.e.called:: <(u32), i64>("foo", (x))
            }
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.e.expect:: <(u32), i64>("foo")
            }
        }
    "#;
    let code = r#"
        ExternalStruct {
            fn foo(&self, x: u32) -> i64;
        }
    "#;
    check(desired, code, do_mock);
}

/// Mocking a generic struct that's defined in another crate
#[test]
fn external_generic_struct() {
    let desired = r#"
        #[derive(Default)]
        struct MockExternalStruct<T: Clone> {
            e: ::mockall::Expectations,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T: Clone> MockExternalStruct<T> {
            pub fn foo(&self, x: u32) -> i64 {
                self.e.called:: <(u32), i64>("foo", (x))
            }
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.e.expect:: <(u32), i64>("foo")
            }
        }
    "#;
    let code = r#"
        ExternalStruct<T: Clone> {
            fn foo(&self, x: u32) -> i64;
        }
    "#;
    check(desired, code, do_mock);
}

/// Mocking a generic struct that's defined in another crate and has a trait
/// impl
#[test]
fn external_generic_struct_with_trait() {
    let desired = r#"
        #[derive(Default)]
        struct MockExternalStruct<T: Clone> {
            e: ::mockall::Expectations,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T: Clone> MockExternalStruct<T> {}
        impl<T: Clone, Q: Copy + 'static> Foo<Q> for MockExternalStruct<T> {
            fn foo(&self, x: Q) -> Q {
                self.e.called:: <(Q), Q>("foo", (x))
            }
        }
        impl<T: Clone, Q: Copy + 'static> MockExternalStruct<T> {
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(Q), Q>
            {
                self.e.expect:: <(Q), Q>("foo")
            }
        }
    "#;
    let code = r#"
        ExternalStruct<T: Clone> {}
        trait Foo<Q: Copy + 'static> {
            fn foo(&self, x: Q) -> Q;
        }
    "#;
    check(desired, code, do_mock);
}

/// Mocking a struct that's defined in another crate, and has a trait
/// implementation
#[test]
fn external_struct_with_trait() {
    let desired = r#"
        #[derive(Default)]
        struct MockExternalStruct {
            e: ::mockall::Expectations,
        }
        impl MockExternalStruct { }
        impl Foo for MockExternalStruct {
            fn foo(&self, x: u32) -> i64 {
                self.e.called:: <(u32), i64>("foo", (x))
            }
        }
        impl MockExternalStruct {
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.e.expect:: <(u32), i64>("foo")
            }
        }
    "#;
    let code = r#"
        ExternalStruct {}
        trait Foo {
            fn foo(&self, x: u32) -> i64;
        }
    "#;
    check(desired, code, do_mock);
}

/// Mocking a struct that's defined in another crate, and has a a trait
/// implementation that includes an associated type
#[test]
fn external_struct_with_trait_with_associated_types() {
    let desired = r#"
        #[derive(Default)]
        struct MockMyIter {
            e: ::mockall::Expectations,
        }
        impl MockMyIter { }
        impl Iterator for MockMyIter {
        type Item=u32;
            fn next(&mut self) -> Option< <Self as Iterator> ::Item> {
                self.e.called:: <(), Option< <Self as Iterator> ::Item> >
                    ("next", ())
            }
        }
        impl MockMyIter {
            pub fn expect_next(&mut self)
                -> &mut ::mockall::Expectation<(),
                    Option< <Self as Iterator> ::Item> >
            {
                self.e.expect:: <(),
                    Option< <Self as Iterator> ::Item> >("next")
            }
        }
    "#;
    let code = r#"
        MyIter {}
        trait Iterator {
            type Item=u32;

            fn next(&mut self) -> Option<<Self as Iterator>::Item>;
        }
    "#;
    check(desired, code, do_mock);
}

#[test]
fn generic_method() {
    check(r#"
    #[derive(Default)]
    struct MockA {
        e: ::mockall::Expectations,
    }
    impl A for MockA {
        fn foo<T: 'static>(&self, t: T) {
            self.e.called:: <(T), ()>("foo", (t))
        }
    }
    impl MockA {
        pub fn expect_foo<T: 'static>(&mut self)
            -> &mut ::mockall::Expectation<(T), ()>
        {
            self.e.expect:: <(T), ()>("foo")
        }
    }"#, r#"
    trait A {
        fn foo<T: 'static>(&self, t: T);
    }"#,
    mock_item);
}

#[test]
fn generic_struct() {
    check(r#"
    #[derive(Default)]
    struct MockGenericStruct< 'a, T, V> {
        e: ::mockall::Expectations,
        _t0: ::std::marker::PhantomData< & 'a ()> ,
        _t1: ::std::marker::PhantomData<T> ,
        _t2: ::std::marker::PhantomData<V> ,
    }"#, r#"
    struct GenericStruct<'a, T, V> {
        t: T,
        v: &'a V
    }"#,
    mock_item);
    check(r#"
    impl< 'a, T, V> MockGenericStruct< 'a, T, V> {
        fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl< 'a, T, V> MockGenericStruct< 'a, T, V> {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl<'a, T, V> GenericStruct<'a, T, V> {
        fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

#[test]
fn generic_struct_with_bounds() {
    check(r#"
    #[derive(Default)]
    struct MockGenericStruct< 'a, T: Copy, V: Clone> {
        e: ::mockall::Expectations,
        _t0: ::std::marker::PhantomData< & 'a ()> ,
        _t1: ::std::marker::PhantomData<T> ,
        _t2: ::std::marker::PhantomData<V> ,
    }"#, r#"
    struct GenericStruct<'a, T: Copy, V: Clone> {
        t: T,
        v: &'a V
    }"#,
    mock_item);
    check(r#"
    impl< 'a, T: Copy, V: Clone> MockGenericStruct< 'a, T, V> {
        fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl< 'a, T: Copy, V: Clone> MockGenericStruct< 'a, T, V> {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl<'a, T: Copy, V: Clone> GenericStruct<'a, T, V> {
        fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

#[test]
fn generic_trait() {
    check(r#"
    #[derive(Default)]
    struct MockGenericTrait<T> {
        e: ::mockall::Expectations,
        _t0: ::std::marker::PhantomData<T> ,
    }
    impl<T> GenericTrait<T> for MockGenericTrait<T> {
        fn foo(&self) {
            self.e.called:: <(), ()>("foo", ())
        }
    }
    impl<T> MockGenericTrait<T> {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
        {
            self.e.expect:: <(), ()>("foo")
        }
    }"#, r#"
    trait GenericTrait<T> {
        fn foo(&self);
    }"#,
    mock_item);
}

#[test]
fn generic_trait_with_bound() {
    check(r#"
    #[derive(Default)]
    struct MockGenericTrait<T: Copy> {
        e: ::mockall::Expectations,
        _t0: ::std::marker::PhantomData<T> ,
    }
    impl<T: Copy> GenericTrait<T> for MockGenericTrait<T> {
        fn foo(&self) {
            self.e.called:: <(), ()>("foo", ())
        }
    }
    impl<T: Copy> MockGenericTrait<T> {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
        {
            self.e.expect:: <(), ()>("foo")
        }
    }"#, r#"
    trait GenericTrait<T: Copy> {
        fn foo(&self);
    }"#,
    mock_item);
}

/// Mock implementing a trait on a structure
#[test]
fn impl_trait() {
    trait Foo {
        fn foo(&self, x: u32) -> i64;
    }
    check(r#"
    impl Foo for MockSomeStruct {
        fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockSomeStruct {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl Foo for SomeStruct {
        fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

/// Mock implementing a trait on a generic structure
#[test]
fn impl_trait_on_generic() {
    trait Foo {
        fn foo(&self, x: u32) -> i64;
    }
    check(r#"
    impl<T> Foo for MockSomeStruct<T> {
        fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl<T> MockSomeStruct<T> {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl<T> Foo for SomeStruct<T> {
        fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

#[test]
fn inherited_trait() {
    trait A {
        fn foo(&self);
    }
    trait B: A {
        fn bar(&self);
    }
    let desired = r#"
    #[derive(Default)]
    struct MockB {
        e: ::mockall::Expectations,
    }
    impl MockB {}
    impl A for MockB {
        fn foo(&self) {
            self.e.called:: <(), ()>("foo", ())
        }
    }
    impl MockB {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
        {
            self.e.expect:: <(), ()>("foo")
        }
    }
    impl B for MockB {
        fn bar(&self) {
            self.e.called:: <(), ()>("bar", ())
        }
    }
    impl MockB {
        pub fn expect_bar(&mut self) -> &mut ::mockall::Expectation<(), ()>
        {
            self.e.expect:: <(), ()>("bar")
        }
    }"#;
    let code = r#"
        B {}
        trait A {
            fn foo(&self);
        }
        trait B {
            fn bar(&self);
        }
    "#;
    check(desired, code, do_mock);
}

#[test]
fn method_by_value() {
    check(r#"
    impl MockMethodByValue {
        fn foo(self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockMethodByValue {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl MethodByValue {
        fn foo(self, x: u32) -> i64 {
            42
        }
    }
    "#,
    mock_item);
}

#[test]
fn pub_crate_struct() {
    check(r#"
    #[derive(Default)]
    pub(crate) struct MockPubCrateStruct {
        e: ::mockall::Expectations,
    }"#, r#"
    pub(crate) struct PubCrateStruct {
        x: i16
    }"#,
    mock_item);
    check(r#"
    impl MockPubCrateStruct {
        pub(crate) fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockPubCrateStruct {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl PubCrateStruct {
        pub(crate) fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

#[test]
fn pub_struct() {
    check(r#"
    #[derive(Default)]
    pub struct MockPubStruct {
        e: ::mockall::Expectations,
    }"#, r#"
    pub struct PubStruct {
        x: i16
    }"#,
    mock_item);
    check(r#"
    impl MockPubStruct {
        pub fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockPubStruct {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl PubStruct {
        pub fn foo(&self, x: u32) -> i64 {
            42
        }
    }
    "#,
    mock_item);
}

#[test]
fn pub_super_struct() {
    check(&r#"
    #[derive(Default)]
    pub(super) struct MockPubSuperStruct {
        e: ::mockall::Expectations,
    }"#, r#"
    pub(super) struct PubSuperStruct {
        x: i16
    }"#,
    mock_item);
    check(&r#"
    impl MockPubSuperStruct {
        pub(super) fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockPubSuperStruct {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl PubSuperStruct {
        pub(super) fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

#[test]
fn simple_struct() {
    check(r#"
    #[derive(Default)]
    struct MockSimpleStruct {
        e: ::mockall::Expectations,
    }"#, r#"
    struct SimpleStruct {
        x: i16
    }"#,
    mock_item);
    check(r#"
    impl MockSimpleStruct {
        fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockSimpleStruct {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#, r#"
    impl SimpleStruct {
        fn foo(&self, x: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}

#[test]
fn simple_trait() {
    check(&r#"
    #[derive(Default)]
    struct MockSimpleTrait {
        e: ::mockall::Expectations,
    }
    impl SimpleTrait for MockSimpleTrait {
        fn foo(&self, x: u32) -> i64 {
            self.e.called:: <(u32), i64>("foo", (x))
        }
    }
    impl MockSimpleTrait {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
        {
            self.e.expect:: <(u32), i64>("foo")
        }
    }"#,
    r#"
    trait SimpleTrait {
        fn foo(&self, x: u32) -> i64;
    }"#,
    mock_item);
}

#[test]
fn static_method() {
    check(&r#"
    #[derive(Default)]
    struct MockA {
        e: ::mockall::Expectations,
    }
    impl A for MockA {
        fn foo(&self, x: u32) -> u32 {
            self.e.called:: <(u32), u32>("foo", (x))
        }
        fn bar() -> u32 {
            unimplemented!("Expectations on static methods are TODO");
        }
    }
    impl MockA {
        pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), u32>
        {
            self.e.expect:: <(u32), u32>("foo")
        }
    }"#,
    r#"
    trait A {
        fn foo(&self, x: u32) -> u32;
        fn bar() -> u32;
    }"#,
    mock_item);
}

#[test]
fn two_args() {
    check(r#"
    #[derive(Default)]
    struct MockTwoArgs {
        e: ::mockall::Expectations,
    }"#, r#"
    struct TwoArgs {}"#,
    mock_item);
    check(r#"
    impl MockTwoArgs {
        fn foo(&self, x: u32, y: u32) -> i64 {
            self.e.called:: <(u32, u32), i64>("foo", (x, y))
        }
    }
    impl MockTwoArgs {
        pub fn expect_foo(&mut self)
            -> &mut ::mockall::Expectation<(u32, u32), i64>
        {
            self.e.expect:: <(u32, u32), i64>("foo")
        }
    }"#, r#"
    impl TwoArgs {
        fn foo(&self, x: u32, y: u32) -> i64 {
            42
        }
    }"#,
    mock_item);
}
}
