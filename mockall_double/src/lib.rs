// vim: tw=80
//! Test double adapter for use with Mockall
//!
//! This crate provides [`[#double]`](macro@double), which can swap in Mock
//! objects for real objects while in test mode.  It's intended to be used in
//! tandem with the [`mockall`](https://docs.rs/mockall/latest/mockall) crate.
//! However, it is defined in its own crate so that the bulk of Mockall can
//! remain a dev-dependency, instead of a regular dependency.

#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]
#![cfg_attr(test, deny(warnings))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    *,
    spanned::Spanned
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
            panic!("{}.  More information may be available when mockall_double is built with the \"nightly\" feature.", msg);
        }
    }
}

fn do_double(_attrs: TokenStream, input: TokenStream) -> TokenStream {
    let mut use_stmt: ItemUse = match parse2(input.clone()) {
        Ok(u) => u,
        Err(e) => return e.to_compile_error()
    };
    mock_itemuse(&mut use_stmt);
    quote!(
        #[cfg(not(test))]
        #input
        #[cfg(test)]
        #use_stmt
    )
}

/// Import a mock type in test mode, or a real type otherwise.
///
/// In a regular build, this macro is a no-op.  But when `#[cfg(test)]`, it
/// substitutes "MockXXX" for any imported "XXX".  This makes it easy to to use
/// mock objects, especially structs, in your unit tests.
///
/// This macro uses the same naming convention as
/// [`mockall`](https://docs.rs/mockall/latest/mockall), but doesn't strictly
/// require it.  It can work with hand-built Mock objects, or any other crate
/// using the same naming convention.
///
/// This is the most common way to use `#[double]`.  In order to replace a type,
/// it must come from a separate module.  So to mock type `Foo`, place it into a
/// submodule, along with its mock counterpart.  Then simply import `Foo` with
/// `#[double]` like this:
/// ```no_run
/// # use mockall_double::double;
/// mod foo {
///     pub(super) struct Foo {
///         // ...
///     }
///     #[cfg(test)]
///     pub(super) struct MockFoo {
///         // ...
///     }
/// }
/// #[double]
/// use foo::Foo;
/// ```
/// That will expand to the following:
/// ```no_run
/// # use mockall_double::double;
/// # mod foo { pub struct Foo {} }
/// #[cfg(not(test))]
/// use foo::Foo;
/// #[cfg(test)]
/// use foo::MockFoo as Foo;
/// ```
/// `#[double]` can handle deeply nested paths,
/// ```no_run
/// # use mockall_double::double;
/// # mod foo { pub mod bar { pub struct Baz{} } }
/// #[double]
/// use foo::bar::Baz;
/// ```
/// grouped imports,
/// ```no_run
/// # mod foo { pub mod bar { pub struct Baz{} pub struct Bean {} } }
/// # use mockall_double::double;
/// #[double]
/// use foo::bar::{
///     Baz,
///     Bean
/// };
/// ```
/// and renamed imports, too.  With renamed imports, it isn't even necessary to
/// declare a submodule.
/// ```no_run
/// # use mockall_double::double;
/// # struct Foo {}
/// #[double]
/// use Foo as Bar;
/// ```
/// Finally, `#[double]` can also import entire mocked modules, not just
/// structures.  In this case the naming convention is different.  It will
/// replace "xxx" with "mock_xxx".  For example:
/// ```no_run
/// # use mockall_double::double;
/// mod foo {
///     pub mod inner {
///         // ...
///     }
///     pub mod mock_inner {
///         // ...
///     }
/// }
/// #[double]
/// use foo::inner;
/// ```
/// will expand to:
/// ```no_run
/// # use mockall_double::double;
/// # mod foo { pub mod inner { } pub mod mock_inner { } }
/// #[cfg(not(test))]
/// use foo::inner;
/// #[cfg(test)]
/// use foo::mock_inner as inner;
/// ```
///
#[proc_macro_attribute]
pub fn double(attrs: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    do_double(attrs.into(), input.into()).into()
}

fn mock_itemuse(orig: &mut ItemUse) {
    if let UseTree::Name(un) = &orig.tree {
        compile_error(un.span(),
            "Cannot double types in the current module.  Use a submodule (use foo::Foo) or a rename (use Foo as Bar)");
    } else {
        mock_usetree(&mut orig.tree)
    }
}

fn mock_ident(i: &Ident) -> Ident {
    let is_type = format!("{}", i)
        .chars()
        .next()
        .expect("zero-length ident?")
        .is_uppercase();
    if is_type {
        // probably a Type
        format_ident!("Mock{}", i)
    } else {
        // probably a module
        format_ident!("mock_{}", i)
    }
}

fn mock_usetree(mut orig: &mut UseTree) {
    match &mut orig {
        UseTree::Glob(star) => {
            compile_error(star.span(),
                "Cannot double glob imports.  Import by fully qualified name instead.");
        },
        UseTree::Group(ug) => {
            for ut in ug.items.iter_mut() {
                mock_usetree(ut);
            }
        },
        UseTree::Name(un) => {
            *orig = UseTree::Rename(UseRename {
                ident: mock_ident(&un.ident),
                as_token: <Token![as]>::default(),
                rename: un.ident.clone()
            });
        },
        UseTree::Path(up) => {
            mock_usetree(up.tree.as_mut());
        },
        UseTree::Rename(ur) => {
            ur.ident = mock_ident(&ur.ident)
        },
    }
}

#[cfg(test)]
mod t {
    use super::*;

mod double {
    use super::*;
    use std::str::FromStr;

    fn cmp(attrs: &str, code: &str, expected: &str) {
        let attrs_ts = TokenStream::from_str(attrs).unwrap();
        let code_ts = TokenStream::from_str(code).unwrap();
        let output = do_double(attrs_ts, code_ts);
        let output = output.to_string();
        // Round-trip expected through proc_macro2 so whitespace will be
        // identically formatted
        let expected = TokenStream::from_str(expected)
            .unwrap()
            .to_string();
        assert_eq!(output, expected);
    }

    #[test]
    #[should_panic(expected = "Cannot double glob")]
    fn glob() {
        let code = r#"use foo::*;"#;
        cmp("", &code, "");
    }

    #[test]
    fn group() {
        let code = r#"
            use foo::bar::{
                Baz,
                Bean
            };
        "#;
        let expected = r#"
            #[cfg(not(test))]
            use foo::bar::{
                Baz,
                Bean
            };
            #[cfg(test)]
            use foo::bar::{
                MockBaz as Baz,
                MockBean as Bean
            };
        "#;
        cmp("", &code, &expected);
    }

    #[test]
    fn module() {
        let code = r#"use foo::bar;"#;
        let expected = r#"
            #[cfg(not(test))]
            use foo::bar;
            #[cfg(test)]
            use foo::mock_bar as bar;
        "#;
        cmp("", &code, &expected);
    }

    #[test]
    #[should_panic(expected = "Cannot double types in the current module")]
    fn name() {
        let code = r#"use Foo;"#;
        cmp("", &code, "");
    }

    #[test]
    fn path() {
        let code = r#"use foo::bar::Baz;"#;
        let expected = r#"
            #[cfg(not(test))]
            use foo::bar::Baz;
            #[cfg(test)]
            use foo::bar::MockBaz as Baz;
        "#;
        cmp("", &code, &expected);
    }

    #[test]
    fn rename() {
        let code = r#"use Foo as Bar;"#;
        let expected = r#"
            #[cfg(not(test))]
            use Foo as Bar;
            #[cfg(test)]
            use MockFoo as Bar;
        "#;
        cmp("", &code, &expected);
    }

    #[test]
    fn not_use_stmt() {
        let code = r#"struct Foo{}"#;
        cmp("", &code, "compile_error!{\"expected `use`\"}");
    }
}
}

