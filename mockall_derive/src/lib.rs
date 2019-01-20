// vim: tw=80
#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]
extern crate proc_macro;

use cfg_if::cfg_if;
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use std::{
    borrow::Borrow,
    collections::HashMap
};
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
        let subs = self.traits.iter().map(|trait_| {
            (trait_.ident.to_string(), trait_.generics.clone())
        }).collect::<Vec<_>>();
        // generate the mock structure
        gen_struct(&self.vis, &self.name, &self.generics, &subs, &self.methods)
            .to_tokens(&mut output);
        // generate sub structures
        for trait_ in self.traits.iter() {
            let sub_mock = syn::Ident::new(
                &format!("{}_{}", &self.name, &trait_.ident),
                Span::call_site());
            let methods = trait_.items.iter().filter_map(|item| {
                if let syn::TraitItem::Method(m) = item {
                    Some(m)
                } else {
                    None
                }
            }).collect::<Vec<_>>();
            gen_struct(&syn::Visibility::Inherited, &sub_mock,
                       &trait_.generics, &[], &methods)
                .to_tokens(&mut output);
        }
        // generate methods on the mock structure itself
        for meth in self.methods.iter() {
            // All mocked methods are public
            let pub_token = syn::token::Pub{span: Span::call_site()};
            let vis = syn::Visibility::Public(syn::VisPublic{pub_token});
            let (mm, em) = gen_mock_method(None, &vis, &meth.sig, None);
            mm.to_tokens(&mut mock_body);
            em.to_tokens(&mut mock_body);
        }
        // generate methods on traits
        let generics = &self.generics;
        let reduced_generics = pathargs_from_generics(&generics);
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

/// Remove the bounds from  a Generics.  Eg:
/// pathargs_from_generics(<'a, T: Copy>) == <'a, T>
/// TODO: try using Generics::split_for_impl instead
fn pathargs_from_generics(g: &syn::Generics) -> syn::PathArguments {
    let mut args = syn::punctuated::Punctuated::new();
    for param in g.params.iter() {
        match param {
            syn::GenericParam::Type(ty) => {
                let mut segments = syn::punctuated::Punctuated::new();
                segments.push(syn::PathSegment::from(ty.ident.clone()));
                let type_path = syn::TypePath {
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments
                    }
                };
                let type_ = syn::Type::Path(type_path);
                args.push(syn::GenericArgument::Type(type_));
            },
            syn::GenericParam::Lifetime(lt) => {
                let newlt = lt.lifetime.clone();
                args.push(syn::GenericArgument::Lifetime(newlt));
            },
            syn::GenericParam::Const(_) => {
                // https://github.com/rust-lang/rust/issues/44580
                compile_error(param.span(),
                    "Generic constants are not yet supported");
            }
        }
    }
    if args.is_empty() {
        syn::PathArguments::None
    } else {
        let abga = syn::AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: syn::Token![<](g.span()),
            args,
            gt_token: syn::Token![>](g.span())
        };
        syn::PathArguments::AngleBracketed(abga)
    }
}

/// Return a type describing the arguments of a method, excluding "self".
/// Also return whether or not the method is a static method.
/// XXX: should this return a syn::TypeTuple instead of a Punctuated?
fn method_input_type(sig: &syn::MethodSig)
    -> (bool, syn::punctuated::Punctuated<syn::Type, Token![,]>)
{
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
    (is_static, input_type)
}

///  Return a type describing the return value of a method
fn method_output_type(sig: &syn::MethodSig) -> syn::Type {
    match &sig.decl.output {
        syn::ReturnType::Default => {
            let paren_token = syn::token::Paren{span: Span::call_site()};
            let elems = syn::punctuated::Punctuated::new();
            syn::Type::Tuple(syn::TypeTuple{paren_token, elems})
        },
        syn::ReturnType::Type(_, ty) => (**ty).clone()
    }
}

/// Generate a mock method and its expectation method
fn gen_mock_method(defaultness: Option<&syn::token::Default>,
                   vis: &syn::Visibility,
                   sig: &syn::MethodSig,
                   sub: Option<&syn::Ident>) -> (TokenStream, TokenStream)
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
    let dedicated = generics.params.is_empty();

    // First the mock method
    quote!(#defaultness #vis #constness #unsafety #asyncness #abi
           #fn_token #ident #generics (#inputs) #output)
        .to_tokens(&mut mock_output);

    let (is_static, input_type) = method_input_type(sig);
    let output_type = method_output_type(sig);
    if is_static {
        quote!({unimplemented!("Expectations on static methods are TODO");})
            .to_tokens(&mut mock_output);
        return (mock_output, TokenStream::new())
    }
    let ident_s = format!("{}", sig.ident);
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

    if !dedicated {
        quote!({
            self.e.called::<(#input_type), #output_type>(#ident_s, (#(#args),*))
        })
    } else if let Some(s) = sub {
        let sub_struct = syn::Ident::new(&format!("{}_expectations", s),
            Span::call_site());
        quote!({
            self.#sub_struct.#ident.call((#(#args),*))
        })
    } else {
        quote!({
            self.#ident.call((#(#args),*))
        })
    }.to_tokens(&mut mock_output);

    // Then the expectation method
    let expect_ident = syn::Ident::new(&format!("expect_{}", sig.ident),
                                       sig.ident.span());
    if !dedicated {
        quote!(pub fn #expect_ident #generics(&mut self)
               -> &mut ::mockall::Expectation<(#input_type), #output_type> {
            self.e.expect::<(#input_type), #output_type>(#ident_s)
       })
    } else if let Some(s) = sub {
        let sub_struct = syn::Ident::new(&format!("{}_expectations", s),
            Span::call_site());
        quote!(pub fn #expect_ident #generics(&mut self)
               -> &mut ::mockall::Expectation<(#input_type), #output_type> {
            self.#sub_struct.#ident = ::mockall::Expectation::new();
            &mut self.#sub_struct.#ident
        })
    } else {
        quote!(pub fn #expect_ident #generics(&mut self)
               -> &mut ::mockall::Expectation<(#input_type), #output_type> {
            self.#ident = ::mockall::Expectation::new();
            &mut self.#ident
        })
    }.to_tokens(&mut expect_output);

    (mock_output, expect_output)
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

fn gen_struct<T>(vis: &syn::Visibility,
                 ident: &syn::Ident,
                 generics: &syn::Generics,
                 subs: &[(String, syn::Generics)],
                 methods: &[T]) -> TokenStream
    where T: Borrow<syn::TraitItemMethod>
{
    let mut output = TokenStream::new();
    let ident = gen_mock_ident(&ident);
    // TODO: don't emit the GenericExpectations object for structs that don't
    // need it.
    let mut body: TokenStream = "e: ::mockall::GenericExpectations,".parse()
        .unwrap();

    // Make Expectation fields for each method
    for (sub, sub_generics) in subs.iter() {
        let spn = Span::call_site();
        let g = pathargs_from_generics(&sub_generics);
        let sub_struct = syn::Ident::new(&format!("{}_expectations", sub), spn);
        let sub_mock = syn::Ident::new(&format!("{}_{}", ident, sub), spn);
        quote!(#sub_struct: #sub_mock #g,).to_tokens(&mut body);
    }
    for meth in methods.iter() {
        if ! meth.borrow().sig.decl.generics.params.is_empty() {
            // Generic methods don't use dedicated expectation objects, because
            // we don't know how they'll be called.
            continue;
        }
        let method_ident = &meth.borrow().sig.ident;
        let (_, in_type) = method_input_type(&meth.borrow().sig);
        let out_type = method_output_type(&meth.borrow().sig);
        quote!(#method_ident: ::mockall::Expectation<(#in_type), #out_type>,)
            .to_tokens(&mut body);
    }

    // Make PhantomData fields, if necessary
    let mut count = 0;
    for param in generics.params.iter() {
        let phname = format!("_t{}", count);
        let phident = syn::Ident::new(&phname, Span::call_site());
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

fn find_ident_from_path(path: &syn::Path) -> (syn::Ident, syn::PathArguments) {
        if path.segments.len() != 1 {
            compile_error(path.span(),
                "mockall_derive only supports structs defined in the current module");
            return (syn::Ident::new("", path.span()), syn::PathArguments::None);
        }
        let last_seg = path.segments.last().unwrap();
        (last_seg.value().ident.clone(), last_seg.value().arguments.clone())
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
                    &meth.sig,
                    Some(&item.ident)
                );
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
                    // mock!
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
    let trait_generics = &item.generics;
    let (merged_g, reduced_struct_g) = match struct_generics {
        None => (trait_generics.clone(),
            pathargs_from_generics(&trait_generics)),
        Some(g) => {
            (g.clone(), pathargs_from_generics(&g))
        }
    };
    let reduced_trait_g = pathargs_from_generics(&trait_generics);
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

fn do_automock(attr_stream: TokenStream, input: TokenStream) -> TokenStream {
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
///     pub Rc<T: 'static> {}
///     trait AsRef<T: 'static> {
///         fn as_ref(&self) -> &'static T;
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

/// Automatically generate mock types for Structs and Traits.
#[proc_macro_attribute]
pub fn automock(attrs: proc_macro::TokenStream, input: proc_macro::TokenStream)
    -> proc_macro::TokenStream
{
    let input: proc_macro2::TokenStream = input.into();
    let mut output = input.clone();
    output.extend(do_automock(attrs.into(), input));
    output.into()
}

/// Test cases for `#[automock]`.
#[cfg(test)]
mod t {

/// Tests for #[automock]
mod automock {
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
            e: ::mockall::GenericExpectations,
            A_expectations: MockA_A,
        }
        #[derive(Default)]
        struct MockA_A {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
            A_expectations : MockA_A ,
        }
        #[derive(Default)]
        struct MockA_A {
            e: ::mockall::GenericExpectations,
        }
        impl MockA {}
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
        }"#);
    }

    #[test]
    fn generic_struct() {
        check("",
        r#"#[derive(Default)]
        pub struct MockGenericStruct< 'a, T, V> {
            e: ::mockall::GenericExpectations ,
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
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
            GenericTrait_expectations: MockGenericTrait_GenericTrait<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        #[derive(Default)]
        struct MockGenericTrait_GenericTrait<T> {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
            GenericTrait_expectations: MockGenericTrait_GenericTrait<T> ,
            _t0: ::std::marker::PhantomData<T> ,
        }
        #[derive(Default)]
        struct MockGenericTrait_GenericTrait<T: Copy> {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations ,
            Foo_expectations: MockSomeStruct_Foo ,
        }
        #[derive(Default)]
        struct MockSomeStruct_Foo {
            e: ::mockall::GenericExpectations ,
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
            e: ::mockall::GenericExpectations,
            Foo_expectations: MockSomeStruct_Foo,
            _t0: ::std::marker::PhantomData<T> ,
        }
        #[derive(Default)]
        struct MockSomeStruct_Foo {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations ,
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
            e: ::mockall::GenericExpectations,
            SimpleTrait_expectations: MockSimpleTrait_SimpleTrait,
        }
        #[derive(Default)]
        struct MockSimpleTrait_SimpleTrait {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
            SimpleTrait_expectations: MockSimpleTrait_SimpleTrait,
        }
        #[derive(Default)]
        struct MockSimpleTrait_SimpleTrait {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
            A_expectations: MockA_A ,
        }
        #[derive(Default)]
        struct MockA_A {
            e: ::mockall::GenericExpectations,
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
            e: ::mockall::GenericExpectations,
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

/// Tests for mock!{}
mod mock {
    use std::str::FromStr;
    use pretty_assertions::assert_eq;
    use super::super::*;

    fn check(desired: &str, code: &str) {
        let ts = proc_macro2::TokenStream::from_str(code).unwrap();
        let output = do_mock(ts).to_string();
        // Let proc_macro2 reformat the whitespace in the expected string
        let expected = proc_macro2::TokenStream::from_str(desired).unwrap()
            .to_string();
        assert_eq!(expected, output);
    }

    /// Mocking a struct with a generic method with mock!{}
    #[test]
    fn generic_method() {
        let desired = r#"
            #[derive(Default)]
            struct MockSomeStruct {
                e: ::mockall::GenericExpectations,
            }
            impl MockSomeStruct {
                pub fn foo<T: 'static>(&self, t: T) {
                    self.e.called:: <(T), ()>("foo", (t))
                }
                pub fn expect_foo<T: 'static>(&mut self)
                    -> &mut ::mockall::Expectation<(T), ()>
                {
                    self.e.expect:: <(T), ()>("foo")
                }
            }
        "#;
        let code = r#"
            SomeStruct {
                fn foo<T: 'static>(&self, t: T);
            }"#;
        check(desired, code);
    }

    /// Mocking a generic struct that's defined in another crate
    #[test]
    fn generic_struct() {
        let desired = r#"
            #[derive(Default)]
            struct MockExternalStruct<T: Clone> {
                e: ::mockall::GenericExpectations,
                foo: ::mockall::Expectation<(u32), i64> ,
                _t0: ::std::marker::PhantomData<T> ,
            }
            impl<T: Clone> MockExternalStruct<T> {
                pub fn foo(&self, x: u32) -> i64 {
                    self.foo.call((x))
                }
                pub fn expect_foo(&mut self)
                    -> &mut ::mockall::Expectation<(u32), i64>
                {
                    self.foo = ::mockall::Expectation::new();
                    &mut self.foo
                }
            }
        "#;
        let code = r#"
            ExternalStruct<T: Clone> {
                fn foo(&self, x: u32) -> i64;
            }
        "#;
        check(desired, code);
    }

    /// Mocking a generic struct that's defined in another crate and has a trait
    /// impl
    #[test]
    fn generic_struct_with_trait() {
        let desired = r#"
            #[derive(Default)]
            struct MockExternalStruct<T: Copy + 'static> {
                e: ::mockall::GenericExpectations,
                Foo_expectations: MockExternalStruct_Foo,
                _t0: ::std::marker::PhantomData<T> ,
            }
            #[derive(Default)]
            struct MockExternalStruct_Foo {
                e: ::mockall::GenericExpectations,
                foo: ::mockall::Expectation<(u32), u32> ,
            }
            impl<T: Copy + 'static> MockExternalStruct<T> {}
            impl<T: Copy + 'static> Foo for MockExternalStruct<T> {
                fn foo(&self, x: u32) -> u32 {
                    self.Foo_expectations.foo.call((x))
                }
            }
            impl<T: Copy + 'static> MockExternalStruct<T> {
                pub fn expect_foo(&mut self)
                    -> &mut ::mockall::Expectation<(u32), u32>
                {
                    self.Foo_expectations.foo = ::mockall::Expectation::new();
                    &mut self.Foo_expectations.foo
                }
            }
        "#;
        let code = r#"
            ExternalStruct<T: Copy + 'static> {}
            trait Foo {
                fn foo(&self, x: u32) -> u32;
            }
        "#;
        check(desired, code);
    }

    /// Implement a generic trait on a generic struct with mock!
    #[test]
    fn generic_struct_with_generic_trait() {
        let desired = r#"
            #[derive(Default)]
            struct MockExternalStruct<T: 'static, Z: 'static> {
                e: ::mockall::GenericExpectations,
                Foo_expectations: MockExternalStruct_Foo<T> ,
                _t0: ::std::marker::PhantomData<T> ,
                _t1: ::std::marker::PhantomData<Z> ,
            }
            #[derive(Default)]
            struct MockExternalStruct_Foo<T: 'static> {
                e: ::mockall::GenericExpectations,
                foo: ::mockall::Expectation<(T), T> ,
                _t0: ::std::marker::PhantomData<T> ,
            }
            impl<T: 'static, Z: 'static> MockExternalStruct<T, Z> {}
            impl<T: 'static, Z: 'static> Foo<T> for MockExternalStruct<T, Z> {
                fn foo(&self, x: T) -> T {
                    self.Foo_expectations.foo.call((x))
                }
            }
            impl<T: 'static, Z: 'static> MockExternalStruct<T, Z> {
                pub fn expect_foo(&mut self)
                    -> &mut ::mockall::Expectation<(T), T>
                {
                    self.Foo_expectations.foo = ::mockall::Expectation::new();
                    &mut self.Foo_expectations.foo
                }
            }
        "#;
        let code = r#"
            ExternalStruct<T: 'static, Z: 'static> {}
            trait Foo<T: 'static> {
                fn foo(&self, x: T) -> T;
            }
        "#;
        check(desired, code);
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
        struct MockExternalStruct {
            e: ::mockall::GenericExpectations,
            A_expectations: MockExternalStruct_A,
            B_expectations: MockExternalStruct_B,
        }
        #[derive(Default)]
        struct MockExternalStruct_A {
            e: ::mockall::GenericExpectations,
            foo: ::mockall::Expectation<(), ()> ,
        }
        #[derive(Default)]
        struct MockExternalStruct_B {
            e: ::mockall::GenericExpectations,
            bar: ::mockall::Expectation<(), ()> ,
        }
        impl MockExternalStruct {}
        impl A for MockExternalStruct {
            fn foo(&self) {
                self.A_expectations.foo.call(())
            }
        }
        impl MockExternalStruct {
            pub fn expect_foo(&mut self) -> &mut ::mockall::Expectation<(), ()>
            {
                self.A_expectations.foo = ::mockall::Expectation::new();
                &mut self.A_expectations.foo
            }
        }
        impl B for MockExternalStruct {
            fn bar(&self) {
                self.B_expectations.bar.call(())
            }
        }
        impl MockExternalStruct {
            pub fn expect_bar(&mut self) -> &mut ::mockall::Expectation<(), ()>
            {
                self.B_expectations.bar = ::mockall::Expectation::new();
                &mut self.B_expectations.bar
            }
        }"#;
        let code = r#"
            ExternalStruct {}
            trait A {
                fn foo(&self);
            }
            trait B {
                fn bar(&self);
            }
        "#;
        check(desired, code);
    }

    /// Mocking a struct that's defined in another crate with mock!
    #[test]
    fn struct_() {
        let desired = r#"
            #[derive(Default)]
            struct MockExternalStruct {
                e: ::mockall::GenericExpectations,
                foo: ::mockall::Expectation<(u32), i64> ,
            }
            impl MockExternalStruct {
                pub fn foo(&self, x: u32) -> i64 {
                    self.foo.call((x))
                }
                pub fn expect_foo(&mut self)
                    -> &mut ::mockall::Expectation<(u32), i64>
                {
                    self.foo = ::mockall::Expectation::new();
                    &mut self.foo
                }
            }
        "#;
        let code = r#"
            ExternalStruct {
                fn foo(&self, x: u32) -> i64;
            }
        "#;
        check(desired, code);
    }

    /// Mocking a struct that's defined in another crate, and has a trait
    /// implementation
    #[test]
    fn struct_with_trait() {
        let desired = r#"
            #[derive(Default)]
            struct MockExternalStruct {
                e: ::mockall::GenericExpectations,
                Foo_expectations: MockExternalStruct_Foo,
            }
            #[derive(Default)]
            struct MockExternalStruct_Foo {
                e: ::mockall::GenericExpectations,
                foo: ::mockall::Expectation<(u32), i64> ,
            }
            impl MockExternalStruct { }
            impl Foo for MockExternalStruct {
                fn foo(&self, x: u32) -> i64 {
                    self.Foo_expectations.foo.call((x))
                }
            }
            impl MockExternalStruct {
                pub fn expect_foo(&mut self)
                    -> &mut ::mockall::Expectation<(u32), i64>
                {
                    self.Foo_expectations.foo = ::mockall::Expectation::new();
                    &mut self.Foo_expectations.foo
                }
            }
        "#;
        let code = r#"
            ExternalStruct {}
            trait Foo {
                fn foo(&self, x: u32) -> i64;
            }
        "#;
        check(desired, code);
    }

    /// Mocking a struct that's defined in another crate, and has a trait
    /// implementation that includes an associated type
    #[test]
    fn struct_with_trait_with_associated_types() {
        let desired = r#"
            #[derive(Default)]
            struct MockMyIter {
                e: ::mockall::GenericExpectations,
                Iterator_expectations: MockMyIter_Iterator,
            }
            #[derive(Default)]
            struct MockMyIter_Iterator {
                e: ::mockall::GenericExpectations,
                next: ::mockall::Expectation<(), Option<u32> > ,
            }
            impl MockMyIter { }
            impl Iterator for MockMyIter {
                type Item=u32;
                fn next(&mut self) -> Option<u32> {
                    self.Iterator_expectations.next.call(())
                }
            }
            impl MockMyIter {
                pub fn expect_next(&mut self)
                    -> &mut ::mockall::Expectation<(), Option<u32> >
                {
                    self.Iterator_expectations.next = ::mockall::Expectation::new();
                    &mut self.Iterator_expectations.next
                }
            }
        "#;
        let code = r#"
            MyIter {}
            trait Iterator {
                type Item=u32;

                fn next(&mut self) -> Option<u32>;
            }
        "#;
        check(desired, code);
    }

}
}
