// vim: tw=80
extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};

/// Generate a mock identifier from the regular one: eg "Foo" => "MockFoo"
fn gen_mock_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("Mock{}", ident), ident.span())
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

    let mut input_type
        = syn::punctuated::Punctuated::<syn::Type, syn::Token![,]>::new();
    for fn_arg in sig.decl.inputs.iter() {
        match fn_arg {
            syn::FnArg::Captured(arg) => input_type.push(arg.ty.clone()),
            syn::FnArg::SelfRef(_) => /* ignore */(),
            syn::FnArg::SelfValue(_) => /* ignore */(),
            _ => unimplemented!(),
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
            _ => panic!("Should be unreachable for normal Rust code")
        }
    }

    quote!({self.e.called::<(#input_type), #output_type>(#ident, (#(#args),*))})
        .to_tokens(&mut mock_output);

    // Then the expectation method
    let mut expect_output = TokenStream::new();
    let expect_ident = syn::Ident::new(&format!("expect_{}", sig.ident),
                                       sig.ident.span());
    quote!(pub fn #expect_ident #generics(&mut self)
           -> ::mockall::ExpectationBuilder<(#input_type), #output_type> {
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
        _ => unimplemented!("This self type is not yet supported by MockAll")
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
            _ => {
                unimplemented!("This impl item is not yet supported by MockAll")
            }
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
                    "#mock does not yet support lifetime bounds on structs");
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
                unimplemented!("#mock does not yet support generic constants");
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

/// Generate a mock struct that implements a trait
fn mock_trait(item: syn::ItemTrait) -> TokenStream {
    let mut output = gen_struct(&item.vis, &item.ident, &item.generics);

    let mut mock_body = TokenStream::new();
    let mut expect_body = TokenStream::new();

    for trait_item in item.items {
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
            _ => {
                unimplemented!("This impl item is not yet supported by MockAll")
            }
        }
    }

    // Put all mock methods in one impl block
    item.unsafety.to_tokens(&mut output);
    let ident = &item.ident;
    let generics = &item.generics;
    let mock_ident = gen_mock_ident(&ident);
    quote!(impl #ident #generics for #mock_ident {#mock_body})
        .to_tokens(&mut output);

    // Put all expect methods in a separate impl block.  This is necessary when
    // mocking a trait impl, where we can't add any new methods
    quote!(impl #generics #mock_ident {#expect_body})
        .to_tokens(&mut output);

    output

}

fn mock_item(input: TokenStream) -> TokenStream {
    let item: syn::Item = match syn::parse(input.into()) {
        Ok(item) => item,
        Err(err) => {
            // TODO: use Span::call_site().error().emit once proc_macro_span is
            // stable
            // https://github.com/rust-lang/rust/issues/54725
            panic!("Failed to parse: {}", err);
        }
    };
    match item {
        syn::Item::Struct(item_struct) => mock_struct(item_struct),
        syn::Item::Impl(item_impl) => mock_impl(item_impl),
        syn::Item::Trait(item_trait) => mock_trait(item_trait),
        _ => unimplemented!("TODO")
    }
}

/// Automatically generate mock types for Structs and Traits.
#[proc_macro_attribute]
pub fn mock(_attr: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let input: proc_macro2::TokenStream = input.into();
    let mut output = input.clone();
    output.extend(mock_item(input));
    output.into()
}

#[cfg(feature = "internal_testing")]
#[proc_macro_attribute]
pub fn expect_mock(attr: proc_macro::TokenStream, item: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let expected = attr.to_string();
    let output = mock_item(item.into()).to_string();
    assert_eq!(expected, output);
    proc_macro::TokenStream::new()
}

/// Test cases for `#[mock]`.
///
/// Proc macros cannot be tested at runtime like normal Rust code.  They can
/// only be tested at compile time.  Run the tests in doc test blocks so that a
/// compile failure by one does not stop the others from running.
///
#[allow(unused)]
#[cfg(feature = "internal_testing")]
mod t {
use super::*;

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// struct MockSimpleStruct {
///     e: ::mockall::Expectations,
/// }
/// )]
/// struct SimpleStruct {
///     x: i16
/// }
/// #[expect_mock(
/// impl MockSimpleStruct {
///     fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockSimpleStruct {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl SimpleStruct {
///     fn foo(&self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type SimpleStruct = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// struct MockTwoArgs {
///     e: ::mockall::Expectations,
/// }
/// )]
/// struct TwoArgs {}
/// #[expect_mock(
/// impl MockTwoArgs {
///     fn foo(&self, x: u32, y: u32) -> i64 {
///         self.e.called::<(u32, u32), i64>("foo", (x, y))
///     }
/// }
/// impl MockTwoArgs {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32, u32), i64>
///     {
///         self.e.expect::<(u32, u32), i64>("foo")
///     }
/// }
/// )]
/// impl TwoArgs {
///     fn foo(&self, x: u32, y: u32) -> i64 {
///         42
///     }
/// }
/// ```
type TwoArgs = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// struct MockA {
///     e: ::mockall::Expectations,
/// }
/// impl A for MockA {
///     fn foo<T>(&self, t: T) {
///         self.e.called::<(T), ()>("foo", (t))
///     }
/// }
/// impl MockA {
///     pub fn expect_foo<T>(&mut self)
///         -> ::mockall::ExpectationBuilder<(T), ()>
///     {
///         self.e.expect::<(T), ()>("foo")
///     }
/// }
/// )]
/// trait A {
///     fn foo<T>(&self, t: T);
/// }
/// ```
type GenericMethod = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// struct MockGenericStruct<'a, T, V> {
///     e: ::mockall::Expectations,
///     _t0: ::std::marker::PhantomData<&'a ()>,
///     _t1: ::std::marker::PhantomData<T>,
///     _t2: ::std::marker::PhantomData<V>,
/// }
/// )]
/// struct GenericStruct<'a, T, V> {
///     t: T,
///     v: &'a V
/// }
/// #[expect_mock(
/// impl<'a, T, V> MockGenericStruct<'a, T, V> {
///     fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl<'a, T, V> MockGenericStruct<'a, T, V> {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl<'a, T, V> GenericStruct<'a, T, V> {
///     fn foo(&self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type GenericStruct = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// impl Foo for MockSomeStruct {
///     fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockSomeStruct {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl Foo for SomeStruct {
///     fn foo(&self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type ImplTrait = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// impl MockMethodByValue {
///     fn foo(self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockMethodByValue {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl MethodByValue {
///     fn foo(self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type MethodByValue = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// pub struct MockPubStruct {
///     e: ::mockall::Expectations,
/// }
/// )]
/// pub struct PubStruct {
///     x: i16
/// }
/// #[expect_mock(
/// impl MockPubStruct {
///     pub fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockPubStruct {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl PubStruct {
///     pub fn foo(&self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type PubStruct = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// pub(crate) struct MockPubCrateStruct {
///     e: ::mockall::Expectations,
/// }
/// )]
/// pub(crate) struct PubCrateStruct {
///     x: i16
/// }
/// #[expect_mock(
/// impl MockPubCrateStruct {
///     pub(crate) fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockPubCrateStruct {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl PubCrateStruct {
///     pub(crate) fn foo(&self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type PubCrateStruct = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// pub(super) struct MockPubSuperStruct {
///     e: ::mockall::Expectations,
/// }
/// )]
/// pub(super) struct PubSuperStruct {
///     x: i16
/// }
/// #[expect_mock(
/// impl MockPubSuperStruct {
///     pub(super) fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockPubSuperStruct {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// impl PubSuperStruct {
///     pub(super) fn foo(&self, x: u32) -> i64 {
///         42
///     }
/// }
/// ```
type PubSuperStruct = ();

/// ```no_run
/// # use mockall_derive::{mock, expect_mock};
/// #[expect_mock(
/// #[derive(Default)]
/// struct MockSimpleTrait {
///     e: ::mockall::Expectations,
/// }
/// impl SimpleTrait for MockSimpleTrait {
///     fn foo(&self, x: u32) -> i64 {
///         self.e.called::<(u32), i64>("foo", (x))
///     }
/// }
/// impl MockSimpleTrait {
///     pub fn expect_foo(&mut self)
///         -> ::mockall::ExpectationBuilder<(u32), i64>
///     {
///         self.e.expect::<(u32), i64>("foo")
///     }
/// }
/// )]
/// trait SimpleTrait {
///     fn foo(&self, x: u32) -> i64;
/// }
/// ```
type SimpleTrait = ();
}
