// vim: tw=80
//! Proc Macros for use with Mockall
//!
//! You probably don't want to use this crate directly.  Instead, you use use
//! its reexports via the [`mockall`](../mockall/index.html) crate.

#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    Token,
    spanned::Spanned
};

mod automock;
mod mock;
use crate::automock::do_automock;
use crate::mock::{Mock, do_mock};

struct MethodTypes {
    is_static: bool,
    input_type: syn::TypeTuple,
    output_type: syn::Type,
    /// Type of Expectation returned by the expect method
    expectation: syn::Type,
    /// Type of Expectations container used by the expect method, without its
    /// generics fields
    expectations: syn::Type,
    /// Type of Expectation object stored in the mock structure, with generics
    /// fields
    expect_obj: syn::Type,
    /// Method to call when invoking the expectation
    call: syn::Ident,
    /// Any turbofish needed when invoking the expectation
    call_turbofish: Option<syn::MethodTurbofish>
}

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

/// Replace any "impl trait" types with "Box<dyn trait>" equivalents
fn deimplify(ty: &mut syn::Type) {
    if let syn::Type::ImplTrait(tit) = ty {
        let bounds = &tit.bounds; // Punctuated<TypeParamBound, Add>
        *ty = syn::parse2(quote!(Box<dyn #bounds>)).unwrap();
    }
}

/// Turn a non-'static reference argument into a pointer argument.  This is
/// sadly necessary because Fn(I) -> O objects aren't 'static unless I is.  And
/// we don't want to restrict our users to use bare fn pointers.
fn derefify(arg: &syn::ArgCaptured) -> (TokenStream, syn::Type) {
    let pat = &arg.pat;
    if let syn::Type::Reference(r) = &arg.ty {
        if let Some(lt) = &r.lifetime {
            if lt.ident == &"static" {
                return (quote!(#pat), arg.ty.clone());
            }
        }
        let ty = r.elem.as_ref();
        if r.mutability.is_some() {
            (quote!(#pat as *mut _), syn::parse2(quote!(*mut #ty)).unwrap())
        } else {
            (quote!(#pat as *const _), syn::parse2(quote!(*const #ty)).unwrap())
        }
    } else {
        (quote!(#pat), arg.ty.clone())
    }
}

/// Replace any references to `Self` in `literal_type` with `actual`.  Useful
/// for constructor methods
fn deselfify(literal_type: &mut syn::Type, actual: &syn::Ident) {
    match literal_type {
        syn::Type::Slice(s) => {
            deselfify(s.elem.as_mut(), actual);
        },
        syn::Type::Array(a) => {
            deselfify(a.elem.as_mut(), actual);
        },
        syn::Type::Ptr(p) => {
            deselfify(p.elem.as_mut(), actual);
        },
        syn::Type::Reference(r) => {
            deselfify(r.elem.as_mut(), actual);
        },
        syn::Type::BareFn(_bfn) => {
            unimplemented!()
        },
        syn::Type::Tuple(tuple) => {
            for elem in tuple.elems.iter_mut() {
                deselfify(elem, actual);
            }
        }
        syn::Type::Path(type_path) => {
            if let Some(ref _qself) = type_path.qself {
                compile_error(type_path.span(), "QSelf is TODO");
            }
            let p = &mut type_path.path;
            for seg in p.segments.iter_mut() {
                if seg.ident == "Self" {
                    seg.ident = actual.clone()
                }
                if let syn::PathArguments::AngleBracketed(abga) =
                    &mut seg.arguments
                {
                    for arg in abga.args.iter_mut() {
                        match arg {
                            syn::GenericArgument::Type(ty) =>
                                deselfify(ty, actual),
                            syn::GenericArgument::Binding(b) =>
                                deselfify(&mut b.ty, actual),
                            _ => /* Nothing to do */(),
                        }
                    }
                }
            }
        },
        syn::Type::Paren(p) => {
            deselfify(p.elem.as_mut(), actual);
        },
        syn::Type::Group(g) => {
            deselfify(g.elem.as_mut(), actual);
        },
        syn::Type::Macro(_) | syn::Type::Verbatim(_) => {
            compile_error(literal_type.span(),
                "mockall_derive does not support this type as a return argument");
        },
        syn::Type::ImplTrait(_) => {
            panic!("deimplify should've already been run on this output type");
        },
        syn::Type::TraitObject(tto) => {
            // Change types like `dyn Self` into `MockXXX`.  For now,
            // don't worry about multiple trait bounds, because they aren't very
            // useful in combination with Self.
            if tto.bounds.len() == 1 {
                if let syn::TypeParamBound::Trait(t)
                    = tto.bounds.first().unwrap().value()
                {
                    let path = &t.path;
                    let mut new_type: syn::Type
                        = syn::parse2(quote!(#path)).unwrap();
                    deselfify(&mut new_type, actual);
                    *literal_type = new_type;
                }
            }
        },
        syn::Type::Infer(_) | syn::Type::Never(_) =>
        {
            /* Nothing to do */
        }
    }
}

/// Generate a mock identifier from the regular one: eg "Foo" => "MockFoo"
fn gen_mock_ident(ident: &syn::Ident) -> syn::Ident {
    syn::Ident::new(&format!("Mock{}", ident), ident.span())
}

fn method_types(mock_ident: Option<&syn::Ident>, sig: &syn::MethodSig)
    -> MethodTypes
{
    let mut is_static = true;
    let mut elems
        = syn::punctuated::Punctuated::<syn::Type, Token![,]>::new();
    let is_generic = !sig.decl.generics.params.is_empty();
    for fn_arg in sig.decl.inputs.iter() {
        match fn_arg {
            syn::FnArg::Captured(arg) => {
                elems.push(derefify(arg).1.clone());
            },
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
    let paren_token = syn::token::Paren::default();
    let input_type = syn::TypeTuple{paren_token, elems};

    let span = Span::call_site();
    let (mut output_type, partial_ex, call) = match &sig.decl.output {
        syn::ReturnType::Default => {
            let paren_token = syn::token::Paren{span};
            let elems = syn::punctuated::Punctuated::new();
            (
                syn::Type::Tuple(syn::TypeTuple{paren_token, elems}),
                syn::Ident::new("Expectation", span),
                syn::Ident::new("call", span)
            )
        },
        syn::ReturnType::Type(_, ty) => {
            match ty.as_ref() {
                syn::Type::Reference(r) => {
                    if let Some(ref lt) = r.lifetime {
                        if lt.ident != &"static" {
                            compile_error(r.span(), "Non-'static non-'self lifetimes are not yet supported");
                        }
                    }
                    if r.mutability.is_some() {
                        (
                            (*r.elem).clone(),
                            syn::Ident::new("RefMutExpectation", span),
                            syn::Ident::new("call_mut", span)
                        )
                    } else {
                        (
                            (*r.elem).clone(),
                            syn::Ident::new("RefExpectation", span),
                            syn::Ident::new("call", span)
                        )
                    }
                },
                _ => (
                    (**ty).clone(),
                    syn::Ident::new("Expectation", span),
                    syn::Ident::new("call", span)
                )
            }
        }
    };
    deimplify(&mut output_type);
    if mock_ident.is_some() {
        deselfify(&mut output_type, &mock_ident.as_ref().unwrap());
    }

    let call_turbofish = if is_generic {
        let mut args = syn::punctuated::Punctuated::new();
        let input_type_type = syn::Type::Tuple(input_type.clone());
        args.push(syn::GenericMethodArgument::Type(input_type_type));
        args.push(syn::GenericMethodArgument::Type(output_type.clone()));
        Some(syn::MethodTurbofish{
            colon2_token: syn::token::Colon2::default(),
            lt_token: syn::token::Lt::default(),
            args,
            gt_token: syn::token::Gt::default()
        })
    } else {
        None
    };
    let expectations_ident = if is_generic {
        syn::Ident::new(&format!("Generic{}s", partial_ex), span)
    } else {
        syn::Ident::new(&format!("{}s", partial_ex), span)
    };
    let expectations = syn::parse2(
        quote!(::mockall::#expectations_ident)
    ).unwrap();
    let expect_obj = if is_generic {
        syn::parse2(quote!(#expectations)).unwrap()
    } else {
        syn::parse2(
            quote!(#expectations<#input_type, #output_type>)
        ).unwrap()
    };
    let expect_ts = quote!(::mockall::#partial_ex);
    let expectation: syn::Type = syn::parse2(expect_ts).unwrap();

    MethodTypes{is_static, input_type, output_type, expectation, expectations,
                call, expect_obj, call_turbofish}
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
/// Mock a trait.  This is the simplest use case.
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
///
/// When mocking a generic struct's implementation of a generic trait, use the
/// same namespace for their generic parameters.  For example, if you wanted to
/// mock `Rc`, do
/// ```
/// # use mockall_derive::mock;
/// mock!{
///     pub Rc<T> {}
///     trait AsRef<T> {
///         fn as_ref(&self) -> &T;
///     }
/// }
/// # fn main() {}
/// ```
/// *not*
/// ```compile_fail
/// # use mockall_derive::mock;
/// mock!{
///     pub Rc<Q> {}
///     trait AsRef<T> {
///         fn as_ref(&self) -> &T;
///     }
/// }
/// # fn main() {}
/// ```
/// Associated types can easily be mocked by specifying a concrete type in the
/// `mock!{}` invocation.  But be careful not to reference the associated type
/// in the signatures of any of the trait's methods; repeat the concrete type
/// instead.  For exampe, do:
/// ```
/// # use mockall_derive::mock;
/// mock!{
///     MyIter {}
///     trait Iterator {
///         type Item=u32;
///
///         fn next(&mut self) -> Option<u32>;
///     }
/// }
/// # fn main() {}
/// ```
/// *not*
/// ```compile_fail
/// # use mockall_derive::mock;
/// mock!{
///     MyIter {}
///     trait Iterator {
///         type Item=u32;
///
///         fn next(&mut self) -> Option<<Self as Iterator>::Item>;
///     }
/// }
/// # fn main() {}
/// ```
#[proc_macro]
pub fn mock(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    do_mock(item.into()).into()
}

/// Automatically generate mock types for structs and traits.
///
/// This is by far the easiest way to use Mockall.  It works on almost all
/// traits, and almost all structs that have a single `impl` block.  In either
/// case, it will generate a mock struct whose name is the name of the mocked
/// struct/trait prepended with "Mock".  For each method of the original, the
/// mock struct will have a method named `expect_whatever` that allows you to
/// set expectations.  There will also be one `checkpoint` method that calls
/// [`checkpoint`] for every single method.
///
/// # Examples
///
/// The simplest use case is mocking a no-frills trait
/// ```
/// # use mockall_derive::*;
/// #[automock]
/// pub trait Foo {
///     fn foo(&self, key: i16);
/// }
///
/// let mock = MockFoo::new();
/// ```
///
/// Mocking a structure:
/// ```
/// # use mockall_derive::*;
/// struct Foo {}
/// #[automock]
/// impl Foo {
///     fn foo(&self) -> u32 {
///         // ...
///         # unimplemented!()
///     }
/// }
/// ```
///
/// Mocking a trait with associated types requires adding a metaitem to the
/// attribute:
/// ```
/// # use mockall_derive::*;
/// #[automock(type Item=u32;)]
/// trait Foo {
///     type Item;
///     fn foo(&self) -> Self::Item;
/// }
/// ```
///
/// Finally, `#[automock]` can also mock foreign functions.  This requires
/// another metaitem to specify the mock module name.
///
/// ```
/// # use mockall_derive::*;
/// #[automock(mod mock_ffi;)]
/// extern "C" {
///     fn foo() -> u32;
/// }
/// ```
///
/// [`checkpoint`]: ../mockall/struct.Expectations.html#method.checkpoint
#[proc_macro_attribute]
pub fn automock(attrs: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let input: proc_macro2::TokenStream = input.into();
    let mut output = input.clone();
    output.extend(do_automock(attrs.into(), input));
    output.into()
}
