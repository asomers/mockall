// vim: tw=80
use super::*;
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Token
};

/// A single automock attribute
enum Attr {
    Type(syn::TraitItemType),
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![type]) {
            input.parse().map(Attr::Type)
        } else {
            Err(lookahead.error())
        }
    }
}

/// automock attributes
struct Attrs {
    attrs: HashMap<syn::Ident, syn::Type>
}

impl Attrs {
    fn get_path(&self, path: &syn::Path) -> Option<syn::Type> {
        if path.leading_colon.is_none() & (path.segments.len() == 2) {
            if path.segments.first().unwrap().value().ident == "Self" {
                let ident = &path.segments.last().unwrap().value().ident;
                self.attrs.get(ident).cloned()
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Recursively substitute types in the input
    fn substitute_type(&self, ty: &mut syn::Type) {
        match ty {
            syn::Type::Slice(s) => {
                self.substitute_type(s.elem.as_mut())
            },
            syn::Type::Array(a) => {
                self.substitute_type(a.elem.as_mut())
            },
            syn::Type::Ptr(p) => {
                self.substitute_type(p.elem.as_mut())
            },
            syn::Type::Reference(r) => {
                self.substitute_type(r.elem.as_mut())
            },
            syn::Type::BareFn(bfn) => {
                for fn_arg in bfn.inputs.iter_mut() {
                    self.substitute_type(&mut fn_arg.ty);
                }
                if let syn::ReturnType::Type(_, ref mut ty) = &mut bfn.output {
                    self.substitute_type(ty);
                }
            },
            syn::Type::Tuple(tuple) => {
                for elem in tuple.elems.iter_mut() {
                    self.substitute_type(elem)
                }
            }
            syn::Type::Path(path) => {
                if let Some(ref _qself) = path.qself {
                    compile_error(path.span(), "QSelf is TODO");
                }
                if let Some(newty) = self.get_path(&path.path) {
                    *ty = newty;
                }
            },
            syn::Type::TraitObject(to) => {
                for bound in to.bounds.iter_mut() {
                    self.substitute_type_param_bound(bound);
                }
            },
            syn::Type::ImplTrait(it) => {
                for bound in it.bounds.iter_mut() {
                    self.substitute_type_param_bound(bound);
                }
            },
            syn::Type::Paren(p) => {
                self.substitute_type(p.elem.as_mut())
            },
            syn::Type::Group(g) => {
                self.substitute_type(g.elem.as_mut())
            },
            syn::Type::Macro(_) | syn::Type::Verbatim(_) => {
                compile_error(ty.span(),
                    "mockall_derive does not support this type when using associated tyeps");
            },
            syn::Type::Infer(_) | syn::Type::Never(_) => {
                /* Nothing to do */
            }
        }
    }

    fn substitute_type_param_bound(&self, bound: &mut syn::TypeParamBound) {
        if let syn::TypeParamBound::Trait(t) = bound {
            match self.get_path(&t.path) {
                None => (), /* Nothing to do */
                Some(syn::Type::Path(type_path)) => {
                    t.path = type_path.path;
                },
                Some(_) => {
                    compile_error(t.path.span(),
                        "Can only substitute paths for trait bounds");
                }
            }
        }
    }

    fn substitute_types(&self, item: &syn::ItemTrait) -> syn::ItemTrait {
        let mut output = item.clone();
        for trait_item in output.items.iter_mut() {
            match trait_item {
                syn::TraitItem::Type(tity) => {
                    if let Some(ty) = self.attrs.get(&tity.ident) {
                        let span = tity.span();
                        tity.default = Some((syn::Token![=](span), ty.clone()));
                        // Concrete associated types aren't allowed to have
                        // bounds
                        tity.bounds = syn::punctuated::Punctuated::new();
                    } else {
                        compile_error(tity.span(),
                            "Default value not given for associated type");
                    }
                },
                syn::TraitItem::Method(method) => {
                    let decl = &mut method.sig.decl;
                    for fn_arg in decl.inputs.iter_mut() {
                        if let syn::FnArg::Captured(arg) = fn_arg {
                            self.substitute_type(&mut arg.ty);
                        }
                    }
                    if let syn::ReturnType::Type(_, ref mut ty) = &mut decl.output {
                        self.substitute_type(ty);
                    }
                },
                _ => {
                    // Nothing to do
                    ()
                }
            }
        }
        output
    }
}

impl Parse for Attrs {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut attrs = HashMap::new();
        while !input.is_empty() {
            let attr: Attr = input.parse()?;
            match attr {
                Attr::Type(trait_item_type) => {
                    let ident = trait_item_type.ident.clone();
                    if let Some((_, ty)) = trait_item_type.default {
                        attrs.insert(ident, ty.clone());
                    } else {
                        compile_error(trait_item_type.span(),
                          "automock type attributes must have a default value");
                    }
                }
            }
        }
        Ok(Attrs{attrs})
    }
}

/// Filter a generics list, keeping only the elements specified by path_args
/// e.g. filter_generics(<A: Copy, B: Clone>, <A>) -> <A: Copy>
fn filter_generics(g: &syn::Generics, path_args: &syn::PathArguments)
    -> syn::Generics
{
    let mut params = syn::punctuated::Punctuated::new();
    match path_args {
        syn::PathArguments::None => ()/* No generics selected */,
        syn::PathArguments::Parenthesized(p) => {
            compile_error(p.span(),
                          "Mockall does not support mocking Fn objects");
        },
        syn::PathArguments::AngleBracketed(abga) => {
            let args = &abga.args;
            if g.where_clause.is_some() {
                compile_error(g.where_clause.span(),
                    "Mockall does not yet support where clauses here");
                return g.clone();
            }
            for param in g.params.iter() {
                match param {
                    syn::GenericParam::Type(tp) => {
                        if args.iter().filter(|ga: &&syn::GenericArgument| {
                            if let syn::GenericArgument::Type(
                                syn::Type::Path(type_path)) = ga
                            {
                                type_path.path.is_ident(tp.ident.clone())
                            } else {
                                false
                            }
                        }).nth(0)
                        .is_some() {
                            params.push(param.clone())
                        }
                    },
                    syn::GenericParam::Lifetime(ld) => {
                        if args.iter().filter(|ga: &&syn::GenericArgument| {
                            if let syn::GenericArgument::Lifetime(lt) = ga {
                                *lt == ld.lifetime
                            } else {
                                false
                            }
                        }).nth(0)
                        .is_some() {
                            params.push(param.clone())
                        }
                    },
                    syn::GenericParam::Const(_) => ()/* Ignore */,
                }
            }
        }
    };
    if params.is_empty() {
        syn::Generics::default()
    } else {
        syn::Generics {
            lt_token: Some(syn::Token![<](g.span())),
            params,
            gt_token: Some(syn::Token![>](g.span())),
            where_clause: None
        }
    }
}

fn find_ident_from_path(path: &syn::Path) -> (syn::Ident, syn::PathArguments) {
        if path.segments.len() != 1 {
            compile_error(path.span(),
                "mockall_derive only supports structs defined in the current module");
            return (syn::Ident::new("", path.span()), syn::PathArguments::None);
        }
        let last_seg = path.segments.last().unwrap();
        (last_seg.value().ident.clone(), last_seg.value().arguments.clone())
}

/// Implement a struct's methods on its mock struct.  Only works if the struct
/// has a single impl block
fn mock_impl(item_impl: syn::ItemImpl) -> TokenStream {
    let name = match *item_impl.self_ty {
        syn::Type::Path(type_path) => {
            find_ident_from_path(&type_path.path).0
        },
        x => {
            compile_error(x.span(),
                "mockall_derive only supports mocking traits and structs");
            return TokenStream::new();
        }
    };
    let methods = item_impl.items.iter().filter_map(|item| {
        match item {
            syn::ImplItem::Const(_) => {
                // const items can easily be added by the user in a separate
                // impl block
                None
            },
            syn::ImplItem::Method(meth) => {
                Some(syn::TraitItemMethod {
                    attrs: Vec::new(),
                    default: None,
                    sig: meth.sig.clone(),
                    semi_token: Some(Token![;](Span::call_site()))
                })
            },
            _ => {
                compile_error(item.span(),
                "This impl item is not yet supported by MockAll");
                None
            }
        }
    }).collect::<Vec<_>>();
    // automock makes everything public
    let pub_token = syn::Token![pub](Span::call_site());
    let vis = syn::Visibility::Public(syn::VisPublic{pub_token});
    let (methods, traits) = if let Some((_, path, _)) = item_impl.trait_ {
        let items = methods.into_iter().map(|meth| {
            syn::TraitItem::Method(meth)
        }).collect::<Vec<_>>();
        let path_args = &path.segments.last().unwrap().value().arguments;
        let trait_ = syn::ItemTrait {
            attrs: Vec::new(),
            vis: vis.clone(),
            unsafety: item_impl.unsafety,
            auto_token: None,
            trait_token: syn::token::Trait::default(),
            ident: find_ident_from_path(&path).0,
            generics: filter_generics(&item_impl.generics, path_args),
            colon_token: None,
            supertraits: syn::punctuated::Punctuated::new(),
            brace_token: syn::token::Brace::default(),
            items
        };
        (Vec::new(), vec![trait_])
    } else {
        (methods, Vec::new())
    };
    let mock = Mock {
        vis,
        name,
        generics: item_impl.generics.clone(),
        methods,
        traits
    };
    mock.gen()
}

/// Generate a mock struct that implements a trait
fn mock_trait(attrs: Attrs, item: syn::ItemTrait) -> TokenStream {
    let generics = item.generics.clone();
    let trait_ = attrs.substitute_types(&item);
    let mock = Mock {
        vis: item.vis.clone(),
        name: item.ident.clone(),
        generics: generics,
        methods: Vec::new(),
        traits: vec![trait_]
    };
    mock.gen()
}

pub(crate)
fn do_automock(attr_stream: TokenStream, input: TokenStream) -> TokenStream
{
    let attrs: Attrs = match syn::parse2(attr_stream) {
        Ok(a) => a,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    let item: syn::Item = match syn::parse2(input) {
        Ok(item) => item,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    match item {
        syn::Item::Impl(item_impl) => mock_impl(item_impl),
        syn::Item::Trait(item_trait) => mock_trait(attrs, item_trait),
        _ => {
            compile_error(item.span(),
                "#[automock] does not support this item type");
            TokenStream::new()
        }
    }
}

/// Test cases for `#[automock]`.
#[cfg(test)]
mod t {
    use std::str::FromStr;
    use pretty_assertions::assert_eq;
    use super::super::*;

    fn check(attrs: &str, desired: &str, code: &str) {
        let attrs_ts = TokenStream::from_str(attrs).unwrap();
        let ts = TokenStream::from_str(code).unwrap();
        let output = do_automock(attrs_ts, ts).to_string();
        // Let proc_macro2 reformat the whitespace in the expected string
        let expected = proc_macro2::TokenStream::from_str(desired).unwrap()
            .to_string();
        assert_eq!(expected, output);
    }

    #[test]
    fn associated_types() {
        check("type T=u32;",
        r#"#[derive(Default)]
        struct MockA {
            A_expectations: MockA_A,
        }
        #[derive(Default)]
        struct MockA_A {
            foo: ::mockall::Expectation<(u32), u32> ,
        }
        impl MockA {}
        impl A for MockA {
            type T = u32;
            fn foo(&self, x: u32) -> u32 {
                self.A_expectations.foo.call((x))
            }
        }
        impl MockA {
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), u32>
            {
                self.A_expectations.foo = ::mockall::Expectation::new();
                &mut self.A_expectations.foo
            }
        }"#, r#"
        trait A {
            type T: Clone + 'static;
            fn foo(&self, x: Self::T) -> Self::T;
        }"#);
    }

    #[test]
    fn generic_method() {
        check("",
        r#"#[derive(Default)]
        struct MockA {
            A_expectations : MockA_A ,
        }
        #[derive(Default)]
        struct MockA_A {
            foo: ::mockall::GenericExpectation,
        }
        impl MockA {}
        impl A for MockA {
            fn foo<T: 'static>(&self, t: T) {
                self.A_expectations.foo.call:: <(T), ()>((t))
            }
        }
        impl MockA {
            pub fn expect_foo<T: 'static>(&mut self)
                -> &mut ::mockall::Expectation<(T), ()>
            {
                self.A_expectations.foo.expect:: <(T), ()>()
            }
        }"#, r#"
        trait A {
            fn foo<T: 'static>(&self, t: T);
        }"#);
    }

    #[test]
    fn generic_struct() {
        check("",
        r#"#[derive(Default)]
        pub struct MockGenericStruct< 'a, T, V> {
            foo: ::mockall::Expectation<(u32), i64> ,
            _t0: ::std::marker::PhantomData< & 'a ()> ,
            _t1: ::std::marker::PhantomData<T> ,
            _t2: ::std::marker::PhantomData<V> ,
        }
        impl< 'a, T, V> MockGenericStruct< 'a, T, V> {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call((x))
            }
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.foo = ::mockall::Expectation::new();
                &mut self.foo
            }
        }"#, r#"
        impl<'a, T, V> GenericStruct<'a, T, V> {
            fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn generic_struct_with_bounds() {
        check("",
        r#"#[derive(Default)]
        pub struct MockGenericStruct< 'a, T: Copy, V: Clone> {
            foo: ::mockall::Expectation<(u32), i64> ,
            _t0: ::std::marker::PhantomData< & 'a ()> ,
            _t1: ::std::marker::PhantomData<T> ,
            _t2: ::std::marker::PhantomData<V> ,
        }
        impl< 'a, T: Copy, V: Clone> MockGenericStruct< 'a, T, V> {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call((x))
            }
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.foo = ::mockall::Expectation::new();
                &mut self.foo
            }
        }"#, r#"
        impl<'a, T: Copy, V: Clone> GenericStruct<'a, T, V> {
            fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn generic_trait() {
        check("",
        r#"#[derive(Default)]
        struct MockGenericTrait<T> {
            GenericTrait_expectations: MockGenericTrait_GenericTrait<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        #[derive(Default)]
        struct MockGenericTrait_GenericTrait<T> {
            foo: ::mockall::Expectation<(), ()> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> MockGenericTrait<T> {}
        impl<T> GenericTrait<T> for MockGenericTrait<T> {
            fn foo(&self) {
                self.GenericTrait_expectations.foo.call(())
            }
        }
        impl<T> MockGenericTrait<T> {
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
            {
                self.GenericTrait_expectations.foo
                    = ::mockall::Expectation::new();
                &mut self.GenericTrait_expectations.foo
            }
        }"#, r#"
        trait GenericTrait<T> {
            fn foo(&self);
        }"#);
    }

    #[test]
    fn generic_trait_with_bound() {
        check("",
        r#"#[derive(Default)]
        struct MockGenericTrait<T: Copy> {
            GenericTrait_expectations: MockGenericTrait_GenericTrait<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        #[derive(Default)]
        struct MockGenericTrait_GenericTrait<T: Copy> {
            foo: ::mockall::Expectation<(), ()> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T: Copy> MockGenericTrait<T> {}
        impl<T: Copy> GenericTrait<T> for MockGenericTrait<T> {
            fn foo(&self) {
                self.GenericTrait_expectations.foo.call(())
            }
        }
        impl<T: Copy> MockGenericTrait<T> {
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
            {
                self.GenericTrait_expectations.foo
                    = ::mockall::Expectation::new();
                &mut self.GenericTrait_expectations.foo
            }
        }"#, r#"
        trait GenericTrait<T: Copy> {
            fn foo(&self);
        }"#);
    }

    /// Mock implementing a trait on a structure
    #[test]
    fn impl_trait() {
        trait Foo {
            fn foo(&self, x: u32) -> i64;
        }
        check("",
        r#"#[derive(Default)]
        pub struct MockSomeStruct {
            Foo_expectations: MockSomeStruct_Foo ,
        }
        #[derive(Default)]
        struct MockSomeStruct_Foo {
            foo: ::mockall::Expectation<(u32), i64> ,
        }
        impl MockSomeStruct { }
        impl Foo for MockSomeStruct {
            fn foo(&self, x: u32) -> i64 {
                self.Foo_expectations.foo.call((x))
            }
        }
        impl MockSomeStruct {
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.Foo_expectations.foo = ::mockall::Expectation::new();
                &mut self.Foo_expectations.foo
            }
        }"#, r#"
        impl Foo for SomeStruct {
            fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    /// Mock implementing a trait on a generic structure
    #[test]
    fn impl_trait_on_generic() {
        trait Foo {
            fn foo(&self, x: u32) -> i64;
        }
        check("",
        r#"#[derive(Default)]
        pub struct MockSomeStruct<T> {
            Foo_expectations: MockSomeStruct_Foo,
            _t0: ::std::marker::PhantomData<T> ,
        }
        #[derive(Default)]
        struct MockSomeStruct_Foo {
            foo: ::mockall::Expectation<(u32), i64> ,
        }
        impl<T> MockSomeStruct<T> {}
        impl<T> Foo for MockSomeStruct<T> {
            fn foo(&self, x: u32) -> i64 {
                self.Foo_expectations.foo.call((x))
            }
        }
        impl<T> MockSomeStruct<T> {
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.Foo_expectations.foo = ::mockall::Expectation::new();
                &mut self.Foo_expectations.foo
            }
        }"#, r#"
        impl<T> Foo for SomeStruct<T> {
            fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn method_by_value() {
        check("",
        r#"#[derive(Default)]
        pub struct MockMethodByValue {
            foo: ::mockall::Expectation<(u32), i64> ,
        }
        impl MockMethodByValue {
            pub fn foo(self, x: u32) -> i64 {
                self.foo.call((x))
            }
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.foo = ::mockall::Expectation::new();
                &mut self.foo
            }
        }"#, r#"
        impl MethodByValue {
            fn foo(self, x: u32) -> i64 {
                42
            }
        }
        "#);
    }

    #[test]
    fn pub_trait() {
        check("",
        &r#"#[derive(Default)]
        pub struct MockSimpleTrait {
            SimpleTrait_expectations: MockSimpleTrait_SimpleTrait,
        }
        #[derive(Default)]
        struct MockSimpleTrait_SimpleTrait {
            foo: ::mockall::Expectation<(), ()> ,
        }
        impl MockSimpleTrait {}
        impl SimpleTrait for MockSimpleTrait {
            fn foo(&self) {
                self.SimpleTrait_expectations.foo.call(())
            }
        }
        impl MockSimpleTrait {
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
            {
                self.SimpleTrait_expectations.foo
                    = ::mockall::Expectation::new();
                &mut self.SimpleTrait_expectations.foo
            }
        }"#,
        r#"
        pub trait SimpleTrait {
            fn foo(&self);
        }"#);
    }

    #[test]
    fn simple_struct() {
        check("",
        r#"#[derive(Default)]
        pub struct MockSimpleStruct {
            foo: ::mockall::Expectation<(u32), i64> ,
        }
        impl MockSimpleStruct {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call((x))
            }
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64>
            {
                self.foo = ::mockall::Expectation::new();
                &mut self.foo
            }
        }"#, r#"
        impl SimpleStruct {
            fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn simple_trait() {
        check("",
        &r#"#[derive(Default)]
        struct MockSimpleTrait {
            SimpleTrait_expectations: MockSimpleTrait_SimpleTrait,
        }
        #[derive(Default)]
        struct MockSimpleTrait_SimpleTrait {
            foo: ::mockall::Expectation<(u32), i64> ,
        }
        impl MockSimpleTrait {}
        impl SimpleTrait for MockSimpleTrait {
            fn foo(&self, x: u32) -> i64 {
                self.SimpleTrait_expectations.foo.call((x))
            }
        }
        impl MockSimpleTrait {
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), i64> {
                self.SimpleTrait_expectations.foo
                    = ::mockall::Expectation::new();
                &mut self.SimpleTrait_expectations.foo
            }
        }"#,
        r#"
        trait SimpleTrait {
            fn foo(&self, x: u32) -> i64;
        }"#);
    }

    #[test]
    fn static_method() {
        check("",
        &r#"#[derive(Default)]
        struct MockA {
            A_expectations: MockA_A ,
        }
        #[derive(Default)]
        struct MockA_A {
            foo: ::mockall::Expectation<(u32), u32> ,
            bar: ::mockall::Expectation<(), u32> ,
        }
        impl MockA {}
        impl A for MockA {
            fn foo(&self, x: u32) -> u32 {
                self.A_expectations.foo.call((x))
            }
            fn bar() -> u32 {
                unimplemented!("Expectations on static methods are TODO");
            }
        }
        impl MockA {
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32), u32>
            {
                self.A_expectations.foo = ::mockall::Expectation::new();
                &mut self.A_expectations.foo
            }
        }"#,
        r#"
        trait A {
            fn foo(&self, x: u32) -> u32;
            fn bar() -> u32;
        }"#);
    }

    #[test]
    fn two_args() {
        check("",
        r#"#[derive(Default)]
        pub struct MockTwoArgs {
            foo: ::mockall::Expectation<(u32, u32), i64> ,
        }
        impl MockTwoArgs {
            pub fn foo(&self, x: u32, y: u32) -> i64 {
                self.foo.call((x, y))
            }
            pub fn expect_foo(&mut self)
                -> &mut ::mockall::Expectation<(u32, u32), i64>
            {
                self.foo = ::mockall::Expectation::new();
                &mut self.foo
            }
        }"#, r#"
        impl TwoArgs {
            fn foo(&self, x: u32, y: u32) -> i64 {
                42
            }
        }"#);
    }

}
