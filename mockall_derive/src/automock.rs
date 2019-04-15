// vim: tw=80
use quote::{ToTokens, quote};
use super::*;
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Token
};

/// A single automock attribute
// This enum is very short-lived, so it's fine not to box it.
#[allow(clippy::large_enum_variant)]
enum Attr {
    Mod(syn::ItemMod),
    Type(syn::TraitItemType),
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![mod]) {
            input.parse().map(Attr::Mod)
        } else if lookahead.peek(Token![type]) {
            input.parse().map(Attr::Type)
        } else {
            Err(lookahead.error())
        }
    }
}

/// automock attributes
#[derive(Debug, Default)]
struct Attrs {
    attrs: HashMap<syn::Ident, syn::Type>,
    modname: Option<syn::Ident>
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

    fn substitute_path_segment(&self, seg: &mut syn::PathSegment) {
        match &mut seg.arguments {
            syn::PathArguments::None => /* nothing to do */(),
            syn::PathArguments::Parenthesized(p) => {
                compile_error(p.span(),
                    "Mockall does not support mocking Fn objects");
            },
            syn::PathArguments::AngleBracketed(abga) => {
                for arg in abga.args.iter_mut() {
                    if let syn::GenericArgument::Type(ty) = arg {
                        self.substitute_type(ty)
                    } else {
                        compile_error(arg.span(),
                            "Mockall does not yet support this argument type in this position");
                    }
                }
            },
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
                } else {
                    for seg in path.path.segments.iter_mut() {
                        self.substitute_path_segment(seg);
                    }
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
                    "mockall_derive does not support this type when using associated types");
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

    fn substitute_trait(&self, item: &syn::ItemTrait) -> syn::ItemTrait {
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
                }
            }
        }
        output
    }
}

impl Parse for Attrs {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let mut attrs = HashMap::new();
        let mut modname = None;
        while !input.is_empty() {
            let attr: Attr = input.parse()?;
            match attr {
                Attr::Mod(item_mod) => {
                    if let Some((br, _)) = item_mod.content {
                        compile_error(br.span,
                            "mod name attributes must have the form \"mod my_name;\"");
                    }
                    modname = Some(item_mod.ident.clone());
                },
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
        Ok(Attrs{attrs, modname})
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
                        let matches = |ga: &syn::GenericArgument| {
                            if let syn::GenericArgument::Type(
                                syn::Type::Path(type_path)) = ga
                            {
                                type_path.path.is_ident(tp.ident.clone())
                            } else {
                                false
                            }
                        };
                        if args.iter().any(matches) {
                            params.push(param.clone())
                        }
                    },
                    syn::GenericParam::Lifetime(ld) => {
                        let matches = |ga: &syn::GenericArgument| {
                            if let syn::GenericArgument::Lifetime(lt) = ga {
                                *lt == ld.lifetime
                            } else {
                                false
                            }
                        };
                        if args.iter().any(matches) {
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

fn mock_foreign(attrs: Attrs, foreign_mod: syn::ItemForeignMod) -> TokenStream {
    let mut body = TokenStream::new();
    let mut cp_body = TokenStream::new();
    let modname = attrs.modname.expect(concat!(
        "module name is required when mocking foreign functions,",
        " like `#[automock(mod mock_ffi)]`"
    ));

    for item in foreign_mod.items {
        match item {
            syn::ForeignItem::Fn(f) => {
                let obj = syn::Ident::new(
                    &format!("{}_expectation", &f.ident),
                    Span::call_site());
                quote!(#obj.lock().unwrap().checkpoint();)
                    .to_tokens(&mut cp_body);
                mock_foreign_function(f).to_tokens(&mut body);
            },
            syn::ForeignItem::Static(s) => {
                // Copy verbatim so a mock method can mutate it
                s.to_tokens(&mut body)
            },
            syn::ForeignItem::Type(ty) => {
                // Copy verbatim
                ty.to_tokens(&mut body)
            },
            syn::ForeignItem::Macro(m) => compile_error(m.span(),
                "Mockall does not support macros in this context"),
            syn::ForeignItem::Verbatim(v) => compile_error(v.span(),
                "Content unrecognized by Mockall"),
        }
    }

    quote!( pub fn checkpoint() { #cp_body }).to_tokens(&mut body);
    quote!(mod #modname { #body })
}

/// Mock a foreign function the same way we mock static trait methods: with a
/// global Expectations object
fn mock_foreign_function(f: syn::ForeignItemFn) -> TokenStream {
    // Foreign functions are always unsafe.  Mock foreign functions should be
    // unsafe too, to prevent "warning: unused unsafe" messages.
    let unsafety = Some(syn::Token![unsafe](f.span()));
    mock_function(&f.vis, None, unsafety, None, &f.ident, &f.decl)
}

fn mock_function(vis: &syn::Visibility,
                 constness: Option<syn::token::Const>,
                 unsafety: Option<syn::token::Unsafe>,
                 asyncness: Option<syn::token::Async>,
                 ident: &syn::Ident,
                 decl: &syn::FnDecl) -> TokenStream
{
    let fn_token = &decl.fn_token;
    let generics = &decl.generics;
    let inputs = &decl.inputs;
    let output = &decl.output;
    let mut args = Vec::new();

    if decl.variadic.is_some() {
        compile_error(decl.variadic.span(),
            "Mockall does not support variadic extern functions");
        return TokenStream::new();
    }

    for p in decl.inputs.iter() {
        match p {
            syn::FnArg::Captured(arg) => {
                args.push(arg.pat.clone());
            },
            _ => compile_error(p.span(),
                "Should be unreachable for normal Rust code")
        }
    }

    let meth_vis = expectation_visibility(&vis, 1);
    let mod_ident = syn::Ident::new(&format!("__{}", &ident), ident.span());
    let sig = syn::MethodSig {
        constness,
        unsafety,
        asyncness,
        abi: None,
        ident: mod_ident.clone(),
        decl: (*decl).clone()
    };
    let meth_types = method_types(&sig, None);
    let altargs = &meth_types.altargs;
    let matchexprs = &meth_types.matchexprs;
    let expect_obj = &meth_types.expect_obj;
    let expect_ident = syn::Ident::new(&format!("expect_{}", &ident),
                                       ident.span());
    let expect_vis = expectation_visibility(&vis, 2);
    let mut g = generics.clone();
    let lt = syn::Lifetime::new("'guard", Span::call_site());
    let ltd = syn::LifetimeDef::new(lt);
    g.params.push(syn::GenericParam::Lifetime(ltd.clone()));

    let obj = syn::Ident::new(
        &format!("{}_expectation", ident),
        Span::call_site());
    // TODO: strip bounds from generics
    quote!(
        ::mockall::expectation!{
            #expect_vis fn #mod_ident<#generics>(#inputs) #output {
                let (#(#altargs),*) = (#(#matchexprs),*);
            }
        }
        ::mockall::lazy_static! {
            static ref #obj: ::std::sync::Mutex<#expect_obj> = 
                ::std::sync::Mutex::new(#mod_ident::Expectations::<#generics>::new());
        }
        #meth_vis #constness #unsafety #asyncness
        #fn_token #ident #generics (#inputs) #output {
            #obj.lock().unwrap().call(#(#args),*)
        }
        #meth_vis fn #expect_ident #g()
               -> #mod_ident::ExpectationGuard<#ltd, #generics>
        {
            #mod_ident::ExpectationGuard::<#generics>::new(#obj.lock().unwrap())
        }
    )
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
    let mut methods = Vec::new();
    let mut titys = Vec::new();
    let mut attrs = Attrs::default();
    for item in item_impl.items.iter() {
        match item {
            syn::ImplItem::Const(_) => {
                // const items can easily be added by the user in a separate
                // impl block
            },
            syn::ImplItem::Method(meth) => {
                methods.push(meth.clone());
            },
            syn::ImplItem::Type(ty) => {
                let tity = syn::TraitItemType {
                    attrs: ty.attrs.clone(),
                    type_token: ty.type_token,
                    ident: ty.ident.clone(),
                    generics: ty.generics.clone(),
                    colon_token: None,
                    bounds: syn::punctuated::Punctuated::new(),
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
    let pub_token = syn::Token![pub](Span::call_site());
    let vis = syn::Visibility::Public(syn::VisPublic{pub_token});
    let (methods, traits) = if let Some((_, path, _)) = item_impl.trait_ {
        let mut items = Vec::new();
        for ty in titys.into_iter() {
            items.push(syn::TraitItem::Type(ty));
        }
        for meth in methods.into_iter() {
            let tim = syn::TraitItemMethod {
                attrs: Vec::new(),
                default: None,
                sig: meth.sig.clone(),
                semi_token: Some(Token![;](Span::call_site()))
            };
            items.push(syn::TraitItem::Method(tim));
        }
        let path_args = &path.segments.last().unwrap().value().arguments;
        let trait_ = syn::ItemTrait {
            attrs: item_impl.attrs.clone(),
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
        let concretized_trait = attrs.substitute_trait(&trait_);
        (Vec::new(), vec![concretized_trait])
    } else {
        assert!(titys.is_empty());
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

/// Generate mock functions for an entire module
fn mock_module(mod_: syn::ItemMod) -> TokenStream {
    let mut body = TokenStream::new();
    let mut cp_body = TokenStream::new();
    let modname = syn::Ident::new(&format!("mock_{}", mod_.ident),
        mod_.ident.span());

    let items = if let Some((_, items)) = mod_.content {
        items
    } else {
        Vec::new()
    };
    for item in items.iter() {
        match item {
            syn::Item::ExternCrate(_) | syn::Item::Use(_)
                | syn::Item::Impl(_) =>
            {
                // Ignore
            },
            syn::Item::Static(is) => {
                is.to_tokens(&mut body)
            },
            syn::Item::Const(ic) => ic.to_tokens(&mut body),
            syn::Item::Fn(f) => {
                let obj = syn::Ident::new(
                    &format!("{}_expectation", &f.ident),
                    Span::call_site());
                quote!(#obj.lock().unwrap().checkpoint();)
                    .to_tokens(&mut cp_body);
                mock_native_function(&f).to_tokens(&mut body);
            },
            syn::Item::Mod(_) | syn::Item::ForeignMod(_)
                | syn::Item::Struct(_) | syn::Item::Enum(_)
                | syn::Item::Union(_) | syn::Item::Trait(_) =>
            {
                compile_error(item.span(),
                    "Mockall does not yet support deriving nested mocks");
            },
            syn::Item::Type(ty) => {
                // Copy verbatim
                ty.to_tokens(&mut body)
            },
            syn::Item::Existential(_) => {
                compile_error(item.span(),
                    "Mockall does not yet support named existential types");
            },
            syn::Item::TraitAlias(ta) => {
                // Copy verbatim
                ta.to_tokens(&mut body)
            },
            syn::Item::Macro(m) => compile_error(m.span(),
                "Mockall does not support macros in this context"),
            syn::Item::Macro2(m) => compile_error(m.span(),
                "Mockall does not support macros in this context"),
            syn::Item::Verbatim(v) => compile_error(v.span(),
                "Content unrecognized by Mockall"),
        }
    }

    quote!(pub fn checkpoint() { #cp_body }).to_tokens(&mut body);
    quote!(mod #modname { #body })
}

/// Mock a function the same way we mock static trait methods: with a
/// global Expectations object
fn mock_native_function(f: &syn::ItemFn) -> TokenStream {
    mock_function(&f.vis, f.constness, f.unsafety, f.asyncness, &f.ident,
                  &f.decl)
}

/// Generate a mock struct that implements a trait
fn mock_trait(attrs: Attrs, item: syn::ItemTrait) -> TokenStream {
    let trait_ = attrs.substitute_trait(&item);
    let mock = Mock {
        vis: item.vis.clone(),
        name: item.ident.clone(),
        generics: item.generics.clone(),
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
        syn::Item::ForeignMod(foreign_mod) => mock_foreign(attrs, foreign_mod),
        syn::Item::Mod(item_mod) => mock_module(item_mod),
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
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
        }
        pub struct MockA {
            A_expectations: MockA_A,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo< >(&self, x: u32) -> u32 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        struct MockA_A {
            foo: __mock_A_A::foo::Expectations ,
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self {
                    foo: __mock_A_A::foo::Expectations::default(),
                }
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            type T = u32;
            fn foo(&self, x: u32) -> u32 {
                self.A_expectations.foo.call(x)
            }
        }
        impl MockA {
            pub fn expect_foo(&mut self)
                -> &mut __mock_A_A::foo::Expectation
            {
                self.A_expectations.foo.expect()
            }
        }"#, r#"
        pub trait A {
            type T: Clone + 'static;
            fn foo(&self, x: Self::T) -> Self::T;
        }"#);
    }

    // Attributes should be copied to the output.  This is useful for stuff like
    // `#[cfg(not(test))]`
    #[test]
    fn attrs() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
            #[bar] ::mockall::expectation!{
                pub fn foo< >(&self) -> () { let () = (); }
            }
            #[baz] ::mockall::expectation!{
                pub fn stat< >() -> () { let () = (); }
            }
        }
        pub struct MockA {
            #[bar] foo: __mock_A::foo::Expectations ,
        }
        #[baz]
        ::mockall::lazy_static! {
            static ref MockA_stat_expectation:
                ::std::sync::Mutex< __mock_A::stat::Expectations >
            = ::std::sync::Mutex::new(__mock_A::stat::Expectations::new());
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    #[bar] foo: __mock_A::foo::Expectations::default(),
                }
            }
        }
        impl MockA {
            #[bar]
            pub fn foo(&self) {
                self.foo.call()
            }
            #[bar]
            pub fn expect_foo(&mut self) -> &mut __mock_A::foo::Expectation{
                self.foo.expect()
            }
            #[baz]
            pub fn stat() {
                MockA_stat_expectation.lock().unwrap().call()
            }
            #[baz]
            pub fn expect_stat< 'guard>()
                -> __mock_A::stat::ExpectationGuard< 'guard>
            {
                __mock_A::stat::ExpectationGuard::new(
                    MockA_stat_expectation.lock().unwrap())
            }
            pub fn checkpoint(&mut self) {
                #[bar] {self.foo.checkpoint();}
                #[baz] {MockA_stat_expectation.lock().unwrap().checkpoint();}
            }
            pub fn new() -> Self { Self::default() }
        }"#;
        let code = r#"
        #[foo]
        impl A {
            #[bar] pub fn foo(&self) {}
            #[baz] pub fn stat() {}
        }"#;
        check("", desired, code);
    }

    #[test]
    fn foreign() {
        let attrs = "mod mock;";
        let desired = r#"
        mod mock {
            ::mockall::expectation!{
                pub(in super::super) fn __foo< >(x: u32) -> i64 {
                    let (p0: &u32) = (&x);
                }
            }
            ::mockall::lazy_static!{
                static ref foo_expectation:
                    ::std::sync::Mutex< __foo::Expectations>
                    = ::std::sync::Mutex::new(__foo::Expectations:: < > ::new());
            }
            pub(in super) unsafe fn foo(x: u32) -> i64 {
                foo_expectation.lock().unwrap().call(x)
            }
            pub(in super) fn expect_foo< 'guard>()
                -> __foo::ExpectationGuard< 'guard, >
            {
                __foo::ExpectationGuard:: < > ::new(foo_expectation.lock().unwrap())
            }
            pub fn checkpoint() {
                foo_expectation.lock().unwrap().checkpoint();
            }
        }
        "#;
        let code = r#"
        extern "C" {
            fn foo(x: u32) -> i64;
        }
        "#;
        check(&attrs, &desired, &code);
    }

    #[test]
    fn foreign_pub() {
        let attrs = "mod mock;";
        let desired = r#"
        mod mock {
            ::mockall::expectation!{
                pub fn __foo< >(x: u32) -> i64 { let (p0: &u32) = (&x);}
            }
            ::mockall::lazy_static!{
                static ref foo_expectation:
                    ::std::sync::Mutex< __foo::Expectations>
                    = ::std::sync::Mutex::new(__foo::Expectations:: < > ::new());
            }
            pub unsafe fn foo(x: u32) -> i64 {
                foo_expectation.lock().unwrap().call(x)
            }
            pub fn expect_foo< 'guard>()
                -> __foo::ExpectationGuard< 'guard, >
            {
                __foo::ExpectationGuard:: < > ::new(foo_expectation.lock().unwrap())
            }
            pub fn checkpoint() {
                foo_expectation.lock().unwrap().checkpoint();
            }
        }
        "#;
        let code = r#"
        extern "C" {
            pub fn foo(x: u32) -> i64;
        }
        "#;
        check(&attrs, &desired, &code);
    }

    #[test]
    fn generic_method() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
        }
        pub struct MockA {
            A_expectations : MockA_A ,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo<T>(&self, t: T) -> () {
                    let (p1: &T) = (&t);
                }
            }
        }
        struct MockA_A {
            foo: __mock_A_A::foo::GenericExpectations,
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self {
                    foo: __mock_A_A::foo::GenericExpectations::default(),
                }
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            fn foo<T: 'static>(&self, t: T) {
                self.A_expectations.foo.call:: <T>(t)
            }
        }
        impl MockA {
            pub fn expect_foo<T: 'static>(&mut self)
                -> &mut __mock_A_A::foo::Expectation<T>
            {
                self.A_expectations.foo.expect:: <T>()
            }
        }"#, r#"
        pub trait A {
            fn foo<T: 'static>(&self, t: T);
        }"#);
    }

    #[test]
    fn generic_struct() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo< 'a, T, V>(&self, x: u32) -> i64 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        pub struct MockFoo< 'a, T, V> {
            foo: __mock_Foo::foo::Expectations< 'a, T, V> ,
            _t0: ::std::marker::PhantomData< & 'a ()> ,
            _t1: ::std::marker::PhantomData<T> ,
            _t2: ::std::marker::PhantomData<V> ,
        }
        impl< 'a, T, V>
        ::std::default::Default for MockFoo< 'a, T, V> {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                    _t1: ::std::marker::PhantomData,
                    _t2: ::std::marker::PhantomData,
                }
            }
        }
        impl< 'a, T, V> MockFoo< 'a, T, V> {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call(x)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation< 'a, T, V>
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl<'a, T, V> Foo<'a, T, V> {
            pub fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn generic_lifetime() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo< 'a>(&self, x: u32) -> i64 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        pub struct MockFoo< 'a> {
            foo: __mock_Foo::foo::Expectations< 'a> ,
            _t0: ::std::marker::PhantomData< & 'a ()> ,
        }
        impl< 'a>
        ::std::default::Default for MockFoo< 'a> {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        impl< 'a> MockFoo< 'a> {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call(x)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation< 'a>
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl<'a> Foo<'a> {
            pub fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn generic_struct_with_bounds() {
        check("", r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo< 'a, T, V>(&self, x: u32) -> i64 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        pub struct MockFoo< 'a, T: Copy, V: Clone> {
            foo: __mock_Foo::foo::Expectations< 'a, T, V> ,
            _t0: ::std::marker::PhantomData< & 'a ()> ,
            _t1: ::std::marker::PhantomData<T> ,
            _t2: ::std::marker::PhantomData<V> ,
        }
        impl< 'a, T: Copy, V: Clone>
        ::std::default::Default for MockFoo< 'a, T, V> {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                    _t1: ::std::marker::PhantomData,
                    _t2: ::std::marker::PhantomData,
                }
            }
        }
        impl< 'a, T: Copy, V: Clone> MockFoo< 'a, T, V> {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call(x)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation< 'a, T, V>
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl<'a, T: Copy, V: Clone> Foo<'a, T, V> {
            pub fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn generic_trait() {
        check("", r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
        }
        pub struct MockFoo<T> {
            Foo_expectations: MockFoo_Foo<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> ::std::default::Default for MockFoo<T> {
            fn default() -> Self {
                Self {
                    Foo_expectations:
                        MockFoo_Foo::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Foo_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo<T>(&self) -> () { let () = (); }
            }
        }
        struct MockFoo_Foo<T> {
            foo: __mock_Foo_Foo::foo::Expectations<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> ::std::default::Default for MockFoo_Foo<T> {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        impl<T> MockFoo_Foo<T> {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl<T> MockFoo<T> {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl<T> Foo<T> for MockFoo<T> {
            fn foo(&self) {
                self.Foo_expectations.foo.call()
            }
        }
        impl<T> MockFoo<T> {
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo_Foo::foo::Expectation<T>
            {
                self.Foo_expectations.foo.expect()
            }
        }"#, r#"
        pub trait Foo<T> {
            fn foo(&self);
        }"#);
    }

    #[test]
    fn generic_trait_with_bound() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
        }
        pub struct MockFoo<T: Copy> {
            Foo_expectations: MockFoo_Foo<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T: Copy> ::std::default::Default for MockFoo<T> {
            fn default() -> Self {
                Self {
                    Foo_expectations:
                        MockFoo_Foo::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Foo_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo<T>(&self) -> () { let () = (); }
            }
        }
        struct MockFoo_Foo<T: Copy> {
            foo: __mock_Foo_Foo::foo::Expectations<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T: Copy>
        ::std::default::Default for MockFoo_Foo<T> {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }

        impl<T: Copy> MockFoo_Foo<T> {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl<T: Copy> MockFoo<T> {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl<T: Copy> Foo<T> for MockFoo<T> {
            fn foo(&self) {
                self.Foo_expectations.foo.call()
            }
        }
        impl<T: Copy> MockFoo<T> {
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo_Foo::foo::Expectation<T>
            {
                self.Foo_expectations.foo
                   .expect()
            }
        }"#, r#"
        pub trait Foo<T: Copy> {
            fn foo(&self);
        }"#);
    }

    /// "impl trait" in a method return value
    #[test]
    fn impl_trait() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo< >(&self) -> Box<dyn Debug + Send> { let () = (); }
            }
        }
        pub struct MockFoo {
            foo: __mock_Foo::foo::Expectations ,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                }
            }
        }
        impl MockFoo {
            pub fn foo(&self) -> impl Debug + Send {
                self.foo.call()
            }
            pub fn expect_foo(&mut self) -> &mut __mock_Foo::foo::Expectation
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl Foo {
            pub fn foo(&self) -> impl Debug + Send {
                42
            }
        }"#);
    }

    /// Mock implementing a trait on a structure
    #[test]
    fn impl_trait_on_struct() {
        trait Foo {
            fn foo(&self, x: u32) -> i64;
        }
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Bar { use super:: * ; }
        pub struct MockBar {
            Foo_expectations: MockBar_Foo ,
        }
        impl ::std::default::Default for MockBar {
            fn default() -> Self {
                Self {
                    Foo_expectations: MockBar_Foo::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Bar_Foo {
            use super:: * ;
            ::mockall::expectation! {
                pub fn foo< >(&self, x: u32) -> i64 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        struct MockBar_Foo {
            foo: __mock_Bar_Foo::foo::Expectations ,
        }
        impl ::std::default::Default for MockBar_Foo {
            fn default() -> Self {
                Self {
                    foo: __mock_Bar_Foo::foo::Expectations::default(),
                }
            }
        }
        impl MockBar_Foo {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl MockBar {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl Foo for MockBar {
            fn foo(&self, x: u32) -> i64 {
                self.Foo_expectations.foo.call(x)
            }
        }
        impl MockBar {
            pub fn expect_foo(&mut self)
                -> &mut __mock_Bar_Foo::foo::Expectation
            {
                self.Foo_expectations.foo.expect()
            }
        }"#, r#"
        impl Foo for Bar {
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
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Bar { use super:: * ; }
        pub struct MockBar<T> {
            Foo_expectations: MockBar_Foo<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> ::std::default::Default for MockBar<T> {
            fn default() -> Self {
                Self {
                    Foo_expectations: MockBar_Foo::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Bar_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo<T>(&self, x: u32) -> i64 { let (p1: &u32) = (&x); }
            }
        }
        struct MockBar_Foo<T> {
            foo: __mock_Bar_Foo::foo::Expectations<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> ::std::default::Default for MockBar_Foo<T> {
            fn default() -> Self {
                Self {
                    foo: __mock_Bar_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        impl<T> MockBar_Foo<T> {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl<T> MockBar<T> {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl<T> Foo for MockBar<T> {
            fn foo(&self, x: u32) -> i64 {
                self.Foo_expectations.foo.call(x)
            }
        }
        impl<T> MockBar<T> {
            pub fn expect_foo(&mut self)
                -> &mut __mock_Bar_Foo::foo::Expectation<T>
            {
                self.Foo_expectations.foo.expect()
            }
        }"#, r#"
        impl<T> Foo for Bar<T> {
            fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    /// Mock implementing a trait with associated types on a struct
    #[test]
    fn impl_trait_with_associated_types() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo{ use super:: * ; }
        pub struct MockFoo {
            Iterator_expectations: MockFoo_Iterator ,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    Iterator_expectations: MockFoo_Iterator::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Foo_Iterator {
            use super:: * ;
            ::mockall::expectation!{
                pub fn next< >(&mut self) -> Option<u32> { let () = (); }
            }
        }
        struct MockFoo_Iterator {
            next: __mock_Foo_Iterator::next::Expectations ,
        }
        impl ::std::default::Default for MockFoo_Iterator {
            fn default() -> Self {
                Self {
                    next: __mock_Foo_Iterator::next::Expectations::default(),
                }
            }
        }
        impl MockFoo_Iterator {
            fn checkpoint(&mut self) {
                {
                    self.next.checkpoint();
                }
            }
        }
        impl MockFoo {
            pub fn checkpoint(&mut self) {
                self.Iterator_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl Iterator for MockFoo {
            type Item = u32;
            fn next(&mut self) -> Option<u32> {
                self.Iterator_expectations.next.call()
            }
        }
        impl MockFoo {
            pub fn expect_next(&mut self)
                -> &mut __mock_Foo_Iterator::next::Expectation
            {
                self.Iterator_expectations.next.expect()
            }
        }"#;
        let code = r#"
        impl Iterator for Foo {
            type Item = u32;

            fn next(&mut self) -> Option<Self::Item> {
                unimplemented!()
            }
        }"#;
        check("", desired, code);
    }

    #[test]
    fn method_by_value() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_MethodByValue{
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo< >(self, x: u32) -> i64 { let (p1: &u32) = (&x); }
            }
        }
        pub struct MockMethodByValue {
            foo: __mock_MethodByValue::foo::Expectations ,
        }
        impl ::std::default::Default for MockMethodByValue {
            fn default() -> Self {
                Self {
                    foo: __mock_MethodByValue::foo::Expectations::default(),
                }
            }
        }
        impl MockMethodByValue {
            pub fn foo(self, x: u32) -> i64 {
                self.foo.call(x)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_MethodByValue::foo::Expectation
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl MethodByValue {
            pub fn foo(self, x: u32) -> i64 {
                42
            }
        }
        "#);
    }

    #[test]
    fn module() {
        let desired = r#"
        mod mock_foo {
            ::mockall::expectation!{
                pub fn __bar< >(x: u32) -> i64 { let (p0: &u32) = (&x); }
            }
            ::mockall::lazy_static!{
                static ref bar_expectation:
                    ::std::sync::Mutex< __bar::Expectations>
                    = ::std::sync::Mutex::new(
                        __bar::Expectations:: < > ::new()
                    );
            }
            pub fn bar(x: u32) -> i64 {
                bar_expectation.lock().unwrap().call(x)
            }
            pub fn expect_bar< 'guard>() -> __bar::ExpectationGuard< 'guard, >
            {
                __bar::ExpectationGuard:: < > ::new( bar_expectation.lock().unwrap())
            }
            pub fn checkpoint() {
                bar_expectation.lock().unwrap().checkpoint();
            }
        }
        "#;
        let code = r#"
        mod foo {
            pub fn bar(x: u32) -> i64 {unimplemented!()}
        }
        "#;
        check(&"", &desired, &code);
    }

    /// In a struct impl block, a method signature can have a mutable argument.
    /// Mockall should ignore the mutability qualifier.
    #[test]
    fn mutable_argument() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo< >(&self, mut x: u32) -> () {
                    let (p1: &u32) = (&x);
                }
            }
            ::mockall::expectation!{
                pub fn bar< >(mut self) -> () { let () = (); }
            }
        }
        pub struct MockFoo {
            foo: __mock_Foo::foo::Expectations ,
            bar: __mock_Foo::bar::Expectations ,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                    bar: __mock_Foo::bar::Expectations::default(),
                }
            }
        }
        impl MockFoo {
            pub fn foo(&self, x: u32) {
                self.foo.call(x)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation
            {
                self.foo.expect()
            }
            pub fn bar(self) {
                self.bar.call()
            }
            pub fn expect_bar(&mut self)
                -> &mut __mock_Foo::bar::Expectation
            {
                self.bar.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
                { self.bar.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#;
        let code = r#"
        impl Foo {
            pub fn foo(&self, mut x: u32) {}
            pub fn bar(mut self) {}
        }"#;
        check("", &desired, &code);
    }

    #[test]
    fn pub_trait() {
        check("",
        &r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo { use super:: * ; }
        pub struct MockFoo {
            Foo_expectations: MockFoo_Foo,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    Foo_expectations: MockFoo_Foo::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Foo_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo< >(&self) -> () { let () = (); }
            }
        }
        struct MockFoo_Foo {
            foo: __mock_Foo_Foo::foo::Expectations ,
        }
        impl ::std::default::Default for MockFoo_Foo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo_Foo::foo::Expectations::default(),
                }
            }
        }
        impl MockFoo_Foo {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl MockFoo {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl Foo for MockFoo {
            fn foo(&self) {
                self.Foo_expectations.foo.call()
            }
        }
        impl MockFoo {
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo_Foo::foo::Expectation
            {
                self.Foo_expectations.foo.expect()
            }
        }"#,
        r#"
        pub trait Foo {
            fn foo(&self);
        }"#);
    }

    #[test]
    fn simple_struct() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo< >(&self, x: u32) -> i64 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        pub struct MockFoo {
            foo: __mock_Foo::foo::Expectations ,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                }
            }
        }
        impl MockFoo {
            pub fn foo(&self, x: u32) -> i64 {
                self.foo.call(x)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl Foo {
            pub fn foo(&self, x: u32) -> i64 {
                42
            }
        }"#);
    }

    // A simple, private trait
    #[test]
    fn simple_trait() {
        check("",
        &r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo { use super:: * ; }
        struct MockFoo {
            Foo_expectations: MockFoo_Foo,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    Foo_expectations: MockFoo_Foo::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Foo_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub(in super::super) fn foo< >(&self, x: u32) -> i64 {
                    let (p1: &u32) = (&x);
                }
            }
        }
        struct MockFoo_Foo {
            foo: __mock_Foo_Foo::foo::Expectations ,
        }
        impl ::std::default::Default for MockFoo_Foo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo_Foo::foo::Expectations::default(),
                }
            }
        }
        impl MockFoo_Foo {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl MockFoo {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl Foo for MockFoo {
            fn foo(&self, x: u32) -> i64 {
                self.Foo_expectations.foo.call(x)
            }
        }
        impl MockFoo {
            fn expect_foo(&mut self)
                -> &mut __mock_Foo_Foo::foo::Expectation {
                self.Foo_expectations.foo.expect()
            }
        }"#,
        r#"
        trait Foo {
            fn foo(&self, x: u32) -> i64;
        }"#);
    }

    #[test]
    fn static_method() {
        check("",
        &r#"
        #[allow(non_snake_case)]
        pub mod __mock_A { use super:: * ; }
        pub struct MockA {
            A_expectations: MockA_A ,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo< >(&self, x: u32) -> u32 {
                    let (p1: &u32) = (&x);
                }
            }
            ::mockall::expectation!{
                pub fn bar< >() -> u32 { let () = (); }
            }
        }
        struct MockA_A {
            foo: __mock_A_A::foo::Expectations ,
        }
        ::mockall::lazy_static!{
            static ref MockA_A_bar_expectation:
                ::std::sync::Mutex< __mock_A_A::bar::Expectations> =
                ::std::sync::Mutex::new(__mock_A_A::bar::Expectations::new());
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self {
                    foo: __mock_A_A::foo::Expectations::default(),
                }
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
                { MockA_A_bar_expectation.lock().unwrap().checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            fn foo(&self, x: u32) -> u32 {
                self.A_expectations.foo.call(x)
            }
            fn bar() -> u32 {
                MockA_A_bar_expectation.lock().unwrap().call()
            }
        }
        impl MockA {
            pub fn expect_foo(&mut self)
                -> &mut __mock_A_A::foo::Expectation
            {
                self.A_expectations.foo.expect()
            }
            pub fn expect_bar< 'guard>()
                -> __mock_A_A::bar::ExpectationGuard< 'guard >
            {
                __mock_A_A::bar::ExpectationGuard::new(
                    MockA_A_bar_expectation.lock().unwrap()
                )
            }
        }"#,
        r#"
        pub trait A {
            fn foo(&self, x: u32) -> u32;
            fn bar() -> u32;
        }"#);
    }

    #[test]
    fn static_constructor_in_struct() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn new< >(x: u32) -> MockA {
                    let (p0: &u32) = (&x);
                }
            }
        }
        pub struct MockA { }
        ::mockall::lazy_static!{
            static ref MockA_new_expectation:
                ::std::sync::Mutex< __mock_A::new::Expectations >
                = ::std::sync::Mutex::new(__mock_A::new::Expectations::new());
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self { }
            }
        }
        impl MockA {
            pub fn new(x: u32) -> Self {
                MockA_new_expectation.lock().unwrap().call(x)
            }
            pub fn expect_new< 'guard>()
                -> __mock_A::new::ExpectationGuard< 'guard>
            {
                __mock_A::new::ExpectationGuard::new(
                    MockA_new_expectation.lock().unwrap()
                )
            }
            pub fn checkpoint(&mut self) {
                { MockA_new_expectation.lock().unwrap().checkpoint(); }
            }
        }"#;
        let code = r#"
        impl A {
            pub fn new(x: u32) -> Self {
                unimplemented!()
            }
        }"#;
        check("", desired, code);
    }

    #[test]
    fn static_constructor_in_trait() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
        }
        pub struct MockA {
            A_expectations: MockA_A ,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn new< >() -> MockA {
                    let () = ();
                }
            }
        }
        struct MockA_A { }
        ::mockall::lazy_static!{
            static ref MockA_A_new_expectation:
                ::std::sync::Mutex< __mock_A_A::new::Expectations >
                = ::std::sync::Mutex::new(__mock_A_A::new::Expectations::new());
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self { }
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { MockA_A_new_expectation.lock().unwrap().checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            fn new() -> Self {
                MockA_A_new_expectation.lock().unwrap().call()
            }
        }
        impl MockA {
            pub fn expect_new< 'guard>()
                -> __mock_A_A::new::ExpectationGuard< 'guard>
            {
                __mock_A_A::new::ExpectationGuard::new(
                    MockA_A_new_expectation.lock().unwrap()
                )
            }
        }"#;
        let code = r#"
        pub trait A {
            fn new() -> Self;
        }"#;
        check("", desired, code);
    }

    #[test]
    fn static_boxed_constructor() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
        }
        pub struct MockA {
            A_expectations: MockA_A,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn new< >() -> Box<MockA> {
                    let () = ();
                }
            }
        }
        struct MockA_A {}
        ::mockall::lazy_static! {
            static ref MockA_A_new_expectation:
            ::std::sync::Mutex< __mock_A_A::new::Expectations >
            = ::std::sync::Mutex::new(__mock_A_A::new::Expectations::new());
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self {}
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { MockA_A_new_expectation.lock().unwrap().checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            fn new() -> Box<Self> {
                MockA_A_new_expectation.lock().unwrap().call()
            }
        }
        impl MockA {
            pub fn expect_new< 'guard>()
                -> __mock_A_A::new::ExpectationGuard< 'guard >
            {
                __mock_A_A::new::ExpectationGuard::new(
                    MockA_A_new_expectation.lock().unwrap()
                )
            }
        }"#;

        let code = r#"
        pub trait A {
            fn new() -> Box<Self>;
        }"#;
        check("", desired, code);
    }

    #[test]
    fn static_impl_trait_constructor() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
        }
        pub struct MockA {
            A_expectations: MockA_A,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn new< >() -> Box<Future<Item=MockA, Error=()> > {
                    let () = ();
                }
            }
        }
        struct MockA_A {}
        ::mockall::lazy_static! {
            static ref MockA_A_new_expectation:
            ::std::sync::Mutex< __mock_A_A::new::Expectations >
            = ::std::sync::Mutex::new(__mock_A_A::new::Expectations::new());
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self {}
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { MockA_A_new_expectation.lock().unwrap().checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            fn new() -> impl Future<Item = Self, Error = ()> {
                MockA_A_new_expectation.lock().unwrap().call()
            }
        }
        impl MockA {
            pub fn expect_new< 'guard>()
                -> __mock_A_A::new::ExpectationGuard< 'guard >
            {
                __mock_A_A::new::ExpectationGuard::new(
                    MockA_A_new_expectation.lock().unwrap()
                )
            }
        }"#;
        let code = r#"
        pub trait A {
            fn new() -> impl Future<Item=Self, Error=()>;
        }"#;
        check("", desired, code);
    }

    #[test]
    fn static_trait_object_constructor() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_A {
            use super:: * ;
        }
        pub struct MockA {
            A_expectations: MockA_A,
        }
        impl ::std::default::Default for MockA {
            fn default() -> Self {
                Self {
                    A_expectations: MockA_A::default(),
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_A_A {
            use super:: * ;
            ::mockall::expectation!{
                pub fn new< >() -> Box<MockA> {
                    let () = ();
                }
            }
        }
        struct MockA_A {}
        ::mockall::lazy_static! {
            static ref MockA_A_new_expectation:
                ::std::sync::Mutex< __mock_A_A::new::Expectations >
                = ::std::sync::Mutex::new(__mock_A_A::new::Expectations::new());
        }
        impl ::std::default::Default for MockA_A {
            fn default() -> Self {
                Self {}
            }
        }
        impl MockA_A {
            fn checkpoint(&mut self) {
                { MockA_A_new_expectation.lock().unwrap().checkpoint(); }
            }
        }
        impl MockA {
            pub fn checkpoint(&mut self) {
                self.A_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl A for MockA {
            fn new() -> Box<dyn Self> {
                MockA_A_new_expectation.lock().unwrap().call()
            }
        }
        impl MockA {
            pub fn expect_new< 'guard>()
                -> __mock_A_A::new::ExpectationGuard< 'guard>
            {
                __mock_A_A::new::ExpectationGuard::new(
                    MockA_A_new_expectation.lock().unwrap()
                )
            }
        }"#;

        // As of rustc 1.33.0-nightly 2019-01-25 this code doesn't even compile.
        // It returns erro E0411 "`Self` is only available in impls, traits, and
        // type definitions".  But it looks like something that might reasonably
        // work someday, so let's make sure we can deselfify it anyway.
        let code = r#"
        pub trait A {
            fn new() -> Box<dyn Self>;
        }"#;
        check("", desired, code);
    }

    #[test]
    fn two_args() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo< >(&self, x: u32, y: u32) -> i64 {
                    let (p1: &u32, p2: &u32) = (&x, &y);
                }
            }
        }
        pub struct MockFoo {
            foo: __mock_Foo::foo::Expectations ,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                }
            }
        }
        impl MockFoo {
            pub fn foo(&self, x: u32, y: u32) -> i64 {
                self.foo.call(x, y)
            }
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation
            {
                self.foo.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl Foo {
            pub fn foo(&self, x: u32, y: u32) -> i64 {
                42
            }
        }"#);
    }

    #[test]
    fn visibility() {
        check("",
        r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub(in super::super) fn foo< >(&self) -> () { let () = (); }
            }
            ::mockall::expectation!{
                pub fn bar< >(&self) -> () { let () = (); }
            }
            ::mockall::expectation!{
                pub(in super::super::super) fn baz< >(&self) -> () {
                    let () = ();
                }
            }
            ::mockall::expectation!{
                pub(crate) fn bang< >(&self) -> () { let () = (); }
            }
            ::mockall::expectation!{
                pub(in super::super::super::x) fn bean< >(&self) -> () {
                    let () = ();
                }
            }
        }
        pub struct MockFoo {
            foo: __mock_Foo::foo::Expectations ,
            bar: __mock_Foo::bar::Expectations ,
            baz: __mock_Foo::baz::Expectations ,
            bang: __mock_Foo::bang::Expectations ,
            bean: __mock_Foo::bean::Expectations ,
        }
        impl ::std::default::Default for MockFoo {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo::foo::Expectations::default(),
                    bar: __mock_Foo::bar::Expectations::default(),
                    baz: __mock_Foo::baz::Expectations::default(),
                    bang: __mock_Foo::bang::Expectations::default(),
                    bean: __mock_Foo::bean::Expectations::default(),
                }
            }
        }
        impl MockFoo {
            fn foo(&self) {
                self.foo.call()
            }
            fn expect_foo(&mut self)
                -> &mut __mock_Foo::foo::Expectation
            {
                self.foo.expect()
            }
            pub fn bar(&self) {
                self.bar.call()
            }
            pub fn expect_bar(&mut self)
                -> &mut __mock_Foo::bar::Expectation
            {
                self.bar.expect()
            }
            pub(super) fn baz(&self) {
                self.baz.call()
            }
            pub(super) fn expect_baz(&mut self)
                -> &mut __mock_Foo::baz::Expectation
            {
                self.baz.expect()
            }
            pub(crate) fn bang(&self) {
                self.bang.call()
            }
            pub(crate) fn expect_bang(&mut self)
                -> &mut __mock_Foo::bang::Expectation
            {
                self.bang.expect()
            }
            pub(in super::x) fn bean(&self) {
                self.bean.call()
            }
            pub(in super::x) fn expect_bean(&mut self)
                -> &mut __mock_Foo::bean::Expectation
            {
                self.bean.expect()
            }
            pub fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
                { self.bar.checkpoint(); }
                { self.baz.checkpoint(); }
                { self.bang.checkpoint(); }
                { self.bean.checkpoint(); }
            }
            pub fn new() -> Self {
                Self::default()
            }
        }"#, r#"
        impl Foo {
            fn foo(&self) {}
            pub fn bar(&self) {}
            pub(super) fn baz(&self) {}
            pub(crate) fn bang(&self) {}
            pub(in super::x) fn bean(&self) {}
        }"#);
    }

    #[test]
    fn where_clause_on_struct() {
        let desired = r#"
            #[allow(non_snake_case)]
            pub mod __mock_Foo {
                use super:: * ;
                ::mockall::expectation!{
                    pub fn foo<T>(&self, x: T) -> T { let (p1: &T) = (&x); }
                }
            }
            pub struct MockFoo<T> where T: Clone {
                foo: __mock_Foo::foo::Expectations<T> ,
                _t0: ::std::marker::PhantomData<T> ,
            }
            impl<T> ::std::default::Default for MockFoo<T> where T: Clone {
                fn default() -> Self {
                    Self {
                        foo: __mock_Foo::foo::Expectations::default(),
                        _t0: ::std::marker::PhantomData,
                    }
                }
            }
            impl<T> MockFoo<T> where T: Clone {
                pub fn foo(&self, x: T) -> T {
                    self.foo.call(x)
                }
                pub fn expect_foo(&mut self)
                    -> &mut __mock_Foo::foo::Expectation<T>
                {
                    self.foo.expect()
                }
                pub fn checkpoint(&mut self) {
                    { self.foo.checkpoint(); }
                }
                pub fn new() -> Self {
                    Self::default()
                }
            }
        "#;
        let code = r#"
            impl<T> Foo<T> where T: Clone {
                pub fn foo(&self, x: T) -> T {
                    x.clone()
                }
            }
        "#;
        check(&"", desired, code);
    }

    #[test]
    fn where_clause_on_trait() {
        let desired = r#"
        #[allow(non_snake_case)]
        pub mod __mock_Foo { use super:: * ; }
        pub struct MockFoo<T> where T: Clone {
            Foo_expectations: MockFoo_Foo<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> ::std::default::Default for MockFoo<T> where T: Clone {
            fn default() -> Self {
                Self {
                    Foo_expectations:
                        MockFoo_Foo::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        #[allow(non_snake_case)]
        pub mod __mock_Foo_Foo {
            use super:: * ;
            ::mockall::expectation!{
                pub fn foo<T>(&self) -> () { let () = (); }
            }
        }
        struct MockFoo_Foo<T> where T: Clone {
            foo: __mock_Foo_Foo::foo::Expectations<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        impl<T> ::std::default::Default for MockFoo_Foo<T> where T: Clone {
            fn default() -> Self {
                Self {
                    foo: __mock_Foo_Foo::foo::Expectations::default(),
                    _t0: ::std::marker::PhantomData,
                }
            }
        }
        impl<T> MockFoo_Foo<T> where T: Clone {
            fn checkpoint(&mut self) {
                { self.foo.checkpoint(); }
            }
        }
        impl<T> MockFoo<T> where T: Clone {
            pub fn checkpoint(&mut self) {
                self.Foo_expectations.checkpoint();
            }
            pub fn new() -> Self {
                Self::default()
            }
        }
        impl<T> Foo<T> for MockFoo<T> where T: Clone {
            fn foo(&self) {
                self.Foo_expectations.foo.call()
            }
        }
        impl<T> MockFoo<T> where T: Clone {
            pub fn expect_foo(&mut self)
                -> &mut __mock_Foo_Foo::foo::Expectation<T>
            {
                self.Foo_expectations.foo.expect()
            }
        }"#;
        let code = r#"
            pub trait Foo<T> where T: Clone {
                fn foo(&self);
            }
        "#;
        check(&"", desired, code);
    }
}
