// vim: tw=80
use super::*;
use quote::ToTokens;
use std::{
    collections::HashMap,
    env
};
use syn::parse::{Parse, ParseStream};

/// A single automock attribute
// This enum is very short-lived, so it's fine not to box it.
#[allow(clippy::large_enum_variant)]
enum Attr {
    Mod(ItemMod),
    Type(TraitItemType),
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> parse::Result<Self> {
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
    attrs: HashMap<Ident, Type>,
    modname: Option<Ident>
}

impl Attrs {
    fn get_path(&self, path: &Path) -> Option<Type> {
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

    fn substitute_path_segment(&self, seg: &mut PathSegment) {
        match &mut seg.arguments {
            PathArguments::None => /* nothing to do */(),
            PathArguments::Parenthesized(p) => {
                compile_error(p.span(),
                    "Mockall does not support mocking Fn objects");
            },
            PathArguments::AngleBracketed(abga) => {
                for arg in abga.args.iter_mut() {
                    match arg {
                        GenericArgument::Type(ty) => {
                            self.substitute_type(ty)
                        },
                        GenericArgument::Binding(binding) => {
                            self.substitute_type(&mut binding.ty);
                        },
                        _ => {
                            compile_error(arg.span(),
                                "Mockall does not yet support this argument type in this position");
                        }
                    }
                }
            },
        }
    }

    /// Recursively substitute types in the input
    fn substitute_type(&self, ty: &mut Type) {
        match ty {
            Type::Slice(s) => {
                self.substitute_type(s.elem.as_mut())
            },
            Type::Array(a) => {
                self.substitute_type(a.elem.as_mut())
            },
            Type::Ptr(p) => {
                self.substitute_type(p.elem.as_mut())
            },
            Type::Reference(r) => {
                self.substitute_type(r.elem.as_mut())
            },
            Type::BareFn(bfn) => {
                for fn_arg in bfn.inputs.iter_mut() {
                    self.substitute_type(&mut fn_arg.ty);
                }
                if let ReturnType::Type(_, ref mut ty) = &mut bfn.output {
                    self.substitute_type(ty);
                }
            },
            Type::Tuple(tuple) => {
                for elem in tuple.elems.iter_mut() {
                    self.substitute_type(elem)
                }
            }
            Type::Path(path) => {
                if let Some(ref qself) = path.qself {
                    let qp = if let Type::Path(p) = qself.ty.as_ref() {
                        &p.path
                    } else {
                        panic!("QSelf's type isn't a path?")
                    };
                    let qident = &qp.segments.first().unwrap().value().ident;
                    if qself.position != 1
                        || qp.segments.len() != 1
                        || path.path.segments.len() != 2
                        || qident != "Self" {
                        compile_error(path.span(),
                            "QSelf is a work in progress");
                    }
                    let last_seg = path.path.segments.pop().unwrap();
                    let to_sub = &last_seg.value().ident;
                    let _ident = path.path.segments.pop().unwrap().value();
                    // TODO: check that the ident is the name of this type
                    let new_type = self.attrs.get(to_sub)
                        .expect("Unknown type substitution for QSelf");
                    *ty = new_type.clone();
                } else if let Some(newty) = self.get_path(&path.path) {
                    *ty = newty;
                } else {
                    for seg in path.path.segments.iter_mut() {
                        self.substitute_path_segment(seg);
                    }
                }
            },
            Type::TraitObject(to) => {
                for bound in to.bounds.iter_mut() {
                    self.substitute_type_param_bound(bound);
                }
            },
            Type::ImplTrait(it) => {
                for bound in it.bounds.iter_mut() {
                    self.substitute_type_param_bound(bound);
                }
            },
            Type::Paren(p) => {
                self.substitute_type(p.elem.as_mut())
            },
            Type::Group(g) => {
                self.substitute_type(g.elem.as_mut())
            },
            Type::Macro(_) | Type::Verbatim(_) => {
                compile_error(ty.span(),
                    "mockall_derive does not support this type when using associated types");
            },
            Type::Infer(_) | Type::Never(_) => {
                /* Nothing to do */
            }
        }
    }

    fn substitute_type_param_bound(&self, bound: &mut TypeParamBound) {
        if let TypeParamBound::Trait(t) = bound {
            match self.get_path(&t.path) {
                None => {
                    for seg in t.path.segments.iter_mut() {
                        self.substitute_path_segment(seg);
                    }
                },
                Some(Type::Path(type_path)) => {
                    t.path = type_path.path;
                },
                Some(_) => {
                    compile_error(t.path.span(),
                        "Can only substitute paths for trait bounds");
                }
            }
        }
    }

    fn substitute_trait(&self, item: &ItemTrait) -> ItemTrait {
        let mut output = item.clone();
        for trait_item in output.items.iter_mut() {
            match trait_item {
                TraitItem::Type(tity) => {
                    if let Some(ty) = self.attrs.get(&tity.ident) {
                        let span = tity.span();
                        tity.default = Some((Token![=](span), ty.clone()));
                        // Concrete associated types aren't allowed to have
                        // bounds
                        tity.bounds = Punctuated::new();
                    } else {
                        compile_error(tity.span(),
                            "Default value not given for associated type");
                    }
                },
                TraitItem::Method(method) => {
                    let decl = &mut method.sig.decl;
                    for fn_arg in decl.inputs.iter_mut() {
                        if let FnArg::Captured(arg) = fn_arg {
                            self.substitute_type(&mut arg.ty);
                        }
                    }
                    if let ReturnType::Type(_, ref mut ty) = &mut decl.output {
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
    fn parse(input: ParseStream) -> parse::Result<Self> {
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
                                type_path.path.is_ident(tp.ident.clone())
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
        (last_seg.value().ident.clone(), last_seg.value().arguments.clone())
}

fn mock_foreign(attrs: Attrs, foreign_mod: ItemForeignMod) -> TokenStream {
    let mut body = TokenStream::new();
    let mut cp_body = TokenStream::new();
    let modname = attrs.modname.expect(concat!(
        "module name is required when mocking foreign functions,",
        " like `#[automock(mod mock_ffi)]`"
    ));

    for item in foreign_mod.items {
        match item {
            ForeignItem::Fn(f) => {
                let obj = Ident::new(
                    &format!("{}_expectation", &f.ident),
                    Span::call_site());
                quote!(
                    let _timeses = #obj.lock().unwrap().checkpoint()
                    .collect::<Vec<_>>();
                ).to_tokens(&mut cp_body);
                mock_foreign_function(f).to_tokens(&mut body);
            },
            ForeignItem::Static(s) => {
                // Copy verbatim so a mock method can mutate it
                s.to_tokens(&mut body)
            },
            ForeignItem::Type(ty) => {
                // Copy verbatim
                ty.to_tokens(&mut body)
            },
            ForeignItem::Macro(m) => compile_error(m.span(),
                "Mockall does not support macros in this context"),
            ForeignItem::Verbatim(v) => compile_error(v.span(),
                "Content unrecognized by Mockall"),
        }
    }

    quote!(pub fn checkpoint() { #cp_body }).to_tokens(&mut body);
    quote!(pub mod #modname { #body })
}

/// Mock a foreign function the same way we mock static trait methods: with a
/// global Expectations object
fn mock_foreign_function(f: ForeignItemFn) -> TokenStream {
    // Foreign functions are always unsafe.  Mock foreign functions should be
    // unsafe too, to prevent "warning: unused unsafe" messages.
    let unsafety = Some(Token![unsafe](f.span()));
    mock_function(&f.vis, None, unsafety, None, &f.ident, &f.decl)
}

fn mock_function(vis: &Visibility,
                 constness: Option<token::Const>,
                 unsafety: Option<token::Unsafe>,
                 asyncness: Option<token::Async>,
                 ident: &Ident,
                 decl: &FnDecl) -> TokenStream
{
    let fn_token = &decl.fn_token;
    let generics = &decl.generics;
    let output = match &decl.output{
        ReturnType::Default => quote!(-> ()),
        _ => {
            let decl_output = &decl.output;
            quote!(#decl_output)
        }
    };
    let mut args = Vec::new();

    if decl.variadic.is_some() {
        compile_error(decl.variadic.span(),
            "Mockall does not support variadic extern functions");
        return TokenStream::new();
    }

    let mod_ident = Ident::new(&format!("__{}", &ident), ident.span());
    let sig = MethodSig {
        constness,
        unsafety,
        asyncness,
        abi: None,
        ident: mod_ident.clone(),
        decl: (*decl).clone()
    };
    let meth_types = method_types(&sig, None);
    let inputs = &meth_types.inputs;

    for p in inputs.iter() {
        match p {
            FnArg::Captured(arg) => {
                args.push(arg.pat.clone());
            },
            _ => compile_error(p.span(),
                "Should be unreachable for normal Rust code")
        }
    }

    let meth_vis = expectation_visibility(&vis, 1);
    let expect_obj = &meth_types.expect_obj;
    let expect_ident = Ident::new(&format!("expect_{}", &ident),
                                       ident.span());
    let expect_vis = expectation_visibility(&vis, 2);
    let mut g = generics.clone();
    let lt = Lifetime::new("'guard", Span::call_site());
    let ltd = LifetimeDef::new(lt);
    g.params.push(GenericParam::Lifetime(ltd.clone()));

    let obj = Ident::new(
        &format!("{}_expectation", ident),
        Span::call_site());
    let mut out = TokenStream::new();
    Expectation::new(&TokenStream::new(), &inputs, None, generics,
        &ident, &mod_ident, None, &decl.output, &expect_vis)
        .to_tokens(&mut out);
    quote!(
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
    ).to_tokens(&mut out);
    out
}

/// Implement a struct's methods on its mock struct.  Only works if the struct
/// has a single impl block
fn mock_impl(item_impl: ItemImpl) -> TokenStream {
    let name = match *item_impl.self_ty {
        Type::Path(type_path) => {
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
        let path_args = &path.segments.last().unwrap().value().arguments;
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
fn mock_module(mod_: ItemMod) -> TokenStream {
    let mut body = TokenStream::new();
    let mut cp_body = TokenStream::new();
    let modname = Ident::new(&format!("mock_{}", mod_.ident),
        mod_.ident.span());

    let items = if let Some((_, items)) = mod_.content {
        items
    } else {
        Vec::new()
    };
    for item in items.iter() {
        match item {
            Item::ExternCrate(_) | Item::Use(_)
                | Item::Impl(_) =>
            {
                // Ignore
            },
            Item::Static(is) => {
                is.to_tokens(&mut body)
            },
            Item::Const(ic) => ic.to_tokens(&mut body),
            Item::Fn(f) => {
                let obj = Ident::new(
                    &format!("{}_expectation", &f.ident),
                    Span::call_site());
                quote!(
                    let _timeses = #obj.lock().unwrap().checkpoint()
                    .collect::<Vec<_>>();
                ).to_tokens(&mut cp_body);
                mock_native_function(&f).to_tokens(&mut body);
            },
            Item::Mod(_) | Item::ForeignMod(_)
                | Item::Struct(_) | Item::Enum(_)
                | Item::Union(_) | Item::Trait(_) =>
            {
                compile_error(item.span(),
                    "Mockall does not yet support deriving nested mocks");
            },
            Item::Type(ty) => {
                // Copy verbatim
                ty.to_tokens(&mut body)
            },
            Item::Existential(_) => {
                compile_error(item.span(),
                    "Mockall does not yet support named existential types");
            },
            Item::TraitAlias(ta) => {
                // Copy verbatim
                ta.to_tokens(&mut body)
            },
            Item::Macro(m) => compile_error(m.span(),
                "Mockall does not support macros in this context"),
            Item::Macro2(m) => compile_error(m.span(),
                "Mockall does not support macros in this context"),
            Item::Verbatim(v) => compile_error(v.span(),
                "Content unrecognized by Mockall"),
        }
    }

    quote!(pub fn checkpoint() { #cp_body }).to_tokens(&mut body);
    quote!(pub mod #modname { #body })
}

/// Mock a function the same way we mock static trait methods: with a
/// global Expectations object
fn mock_native_function(f: &ItemFn) -> TokenStream {
    mock_function(&f.vis, f.constness, f.unsafety, f.asyncness, &f.ident,
                  &f.decl)
}

/// Generate a mock struct that implements a trait
fn mock_trait(attrs: Attrs, item: ItemTrait) -> TokenStream {
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
    let attrs: Attrs = match parse2(attr_stream) {
        Ok(a) => a,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    let item: Item = match parse2(input) {
        Ok(item) => item,
        Err(err) => {
            return err.to_compile_error();
        }
    };
    let ts = match item {
        Item::Impl(item_impl) => mock_impl(item_impl),
        Item::ForeignMod(foreign_mod) => mock_foreign(attrs, foreign_mod),
        Item::Mod(item_mod) => mock_module(item_mod),
        Item::Trait(item_trait) => mock_trait(attrs, item_trait),
        _ => {
            compile_error(item.span(),
                "#[automock] does not support this item type");
            TokenStream::new()
        }
    };
    if env::var("MOCKALL_DEBUG").is_ok() {
        println!("{}", ts);
    }
    ts
}

/// Test cases for `#[automock]`.
#[cfg(test)]
mod t {
    use std::str::FromStr;
    use pretty_assertions::assert_eq;
    use super::super::*;

    fn check_substitute_type(attrs: TokenStream, input: TokenStream,
        expected: TokenStream)
    {
        let _self: super::Attrs = parse2(attrs).unwrap();
        let mut in_ty: Type = parse2(input).unwrap();
        let expect_ty: Type = parse2(expected).unwrap();
        _self.substitute_type(&mut in_ty);
        assert_eq!(in_ty, expect_ty);
    }

    #[test]
    fn qself() {
        check_substitute_type(quote!(type T = u32;),
                              quote!(<Self as Foo>::T),
                              quote!(u32));
    }

    #[test]
    fn method_visibility() {
        let code = r#"
        impl Foo {
            fn foo(&self) {}
            pub fn bar(&self) {}
            pub(super) fn baz(&self) {}
            pub(crate) fn bang(&self) {}
            pub(in super::x) fn bean(&self) {}
        }"#;
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let attrs_ts = proc_macro2::TokenStream::from_str("").unwrap();
        let output = do_automock(attrs_ts, ts).to_string();
        assert!(!output.contains("pub fn foo"));
        assert!(!output.contains("pub fn expect_foo"));
        assert!(output.contains("pub fn bar"));
        assert!(output.contains("pub fn expect_bar"));
        assert!(output.contains("pub ( super ) fn baz"));
        assert!(output.contains("pub ( super ) fn expect_baz"));
        assert!(output.contains("pub ( crate ) fn bang"));
        assert!(output.contains("pub ( crate ) fn expect_bang"));
        assert!(output.contains("pub ( in super :: x ) fn bean"));
        assert!(output.contains("pub ( in super :: x ) fn expect_bean"));
    }

    #[test]
    fn trait_visibility() {
        let code = r#"
        pub(super) trait Foo {}
        "#;
        let attrs_ts = proc_macro2::TokenStream::from_str("").unwrap();
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let output = do_automock(attrs_ts, ts).to_string();
        assert!(output.contains("pub ( super ) struct MockFoo"));
    }
}
