// vim: tw=80
use super::*;

use quote::ToTokens;

use crate::{
    mockable_item::{MockableItem, MockableModule}
};

/// A Mock item
pub(crate) enum MockItem {
    Module(MockItemModule)
}

impl From<MockableItem> for MockItem {
    fn from(mockable: MockableItem) -> MockItem {
        match mockable {
            MockableItem::Manual(_) => unimplemented!("1"),
            MockableItem::Module(mod_) => MockItem::Module(
                MockItemModule::from(mod_)
            )
        }
    }
}

impl ToTokens for MockItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MockItem::Module(mod_) => mod_.to_tokens(tokens),
            _ => unimplemented!("2")
        }
    }
}

struct MockFunction {
    /// Lifetime Generics of the mocked method that relate to the arguments but
    /// not the return value
    alifetimes: Generics,
    /// Names of the method arguments
    argnames: Vec<Pat>,
    /// Types of the method arguments
    argty: Vec<Type>,
    /// Type Generics of the Expectation object
    egenerics: Generics,
    /// The mock function's generic types as a list of types
    fn_params: Punctuated<Ident, Token![,]>,
    /// Is this for a static method or free function?
    is_static: bool,
    /// The mockable version of the function
    item_fn: ItemFn,
    /// name of the function's parent module
    mod_ident: Ident,
    /// Output type of the Method, supersuperfied.
    output: Type,
    /// Expressions that create the predicate arguments from the call arguments
    predexprs: Vec<TokenStream>,
    /// Types used for Predicates.  Will be almost the same as args, but every
    /// type will be a non-reference type.
    predty: Vec<Type>,
    /// References to every type in `predty`.
    refpredty: Vec<Type>
}

impl MockFunction {
    // TODO: return a Syn object instead of a TokenStream
    fn hrtb(&self) -> TokenStream {
        if self.alifetimes.params.is_empty() {
            TokenStream::default()
        } else {
            let ig = &self.alifetimes;
            quote!(for #ig)
        }
    }

}

impl From<(&Ident, ItemFn)> for MockFunction {
    /// Create a MockFunction from its mockable version and the name of its
    /// parent module
    fn from((ident, f): (&Ident, ItemFn)) -> MockFunction {
        let egenerics = Generics::default();    // TODO
        let mut argnames = Vec::new();
        let mut argty = Vec::new();
        let mut is_static = true;
        let mut predexprs = Vec::new();
        let mut predty = Vec::new();
        let mut refpredty = Vec::new();
        for fa in f.sig.inputs.iter() {
            if let FnArg::Typed(pt) = fa {
                let argname = (*pt.pat).clone();
                let aty = &pt.ty;
                //let aty = supersuperfy(&pt.ty, levels);
                if let Type::Reference(tr) = &**aty {
                    predexprs.push(quote!(#argname));
                    predty.push((*tr.elem).clone());
                    refpredty.push((**aty).clone());
                } else {
                    predexprs.push(quote!(&#argname));
                    predty.push((**aty).clone());
                    let tr = TypeReference {
                        and_token: Token![&](Span::call_site()),
                        lifetime: None,
                        mutability: None,
                        elem: Box::new((**aty).clone())
                    };
                    refpredty.push(Type::Reference(tr));
                };
                argnames.push(argname);
                argty.push((**aty).clone());
            } else {
                is_static = false;
                ()    // Strip out the "&self" argument
            }
        }
        // TODO: supersuperfy the output
        let output = match f.sig.output.clone() {
            ReturnType::Default => Type::Tuple(TypeTuple{
                paren_token: token::Paren(Span::call_site()),
                elems: Punctuated::new()
            }),
            ReturnType::Type(_, ty) => (*ty).clone()
        };
        let (mut egenerics, alifetimes, rlifetimes) = split_lifetimes(
            f.sig.generics.clone(),
            &f.sig.inputs,
            &f.sig.output
        );
        let fn_params = Punctuated::<Ident, Token![,]>::from_iter(
            egenerics.type_params().map(|tp| tp.ident.clone())
        );
        MockFunction {
            alifetimes,
            argnames,
            argty,
            egenerics,
            fn_params,
            is_static,
            item_fn: f,
            mod_ident: ident.clone(),
            output,
            predexprs,
            predty,
            refpredty,
        }
    }
}

impl ToTokens for MockFunction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let matcher = &Matcher{f: self};
        let rfunc = &Rfunc{f: self};
        let mod_ident = format_ident!("__{}", &self.item_fn.sig.ident);
        let orig_signature = &self.item_fn.sig;
        quote!(
            #[allow(missing_docs)]
            pub mod #mod_ident {
                use super::*;
                use ::mockall::CaseTreeExt;
                use ::std::sync::MutexGuard;
                use ::std::{
                    mem,
                    ops::{DerefMut, Range},
                    sync::Mutex
                };
                #rfunc
                #matcher
                struct Common{} // 106 lines
                pub struct Expectation{}    // 208 lines
                pub struct Expectations{}   // 43 lines
                pub struct ExpectationGuard {} // 114 lines
                pub struct Context {}   // 44 lines
            }
            #orig_signature {}
            pub fn bar_context() {}
        ).to_tokens(tokens);
    }
}

struct Matcher<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for Matcher<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let argnames = &self.f.argnames;
        let braces = argnames.iter()
            .fold(String::new(), |mut acc, _argname| {
                if acc.is_empty() {
                    acc.push_str("{}");
                } else {
                    acc.push_str(", {}");
                }
                acc
            });
        let fn_params = &self.f.fn_params;
        let hrtb = self.f.hrtb();
        let indices = (0..argnames.len())
            .map(|i| {
                syn::Index::from(i)
            }).collect::<Vec<_>>();
        let lg = &self.f.alifetimes;
        let pred_matches = TokenStream::from_iter(
            argnames.iter().enumerate()
            .map(|(i, argname)| {
                let idx = syn::Index::from(i);
                quote!(__mockall_pred.#idx.eval(#argname),)
            })
        );
        let preds = TokenStream::from_iter(
            self.f.predty.iter().map(|t|
                quote!(Box<dyn #hrtb ::mockall::Predicate<#t> + Send>,)
            )
        );
        let predty = &self.f.predty;
        let refpredty = &self.f.refpredty;
        quote!(
            enum Matcher #ig #wc {
                Always,
                Func(Box<dyn #hrtb Fn(#( #refpredty, )*) -> bool + Send>),
                // Version of Matcher::Func for closures that aren't Send
                FuncST(::mockall::Fragile<Box<dyn #hrtb Fn(#( #refpredty, )*) -> bool>>),
                Pred(Box<(#preds)>),
                // Prevent "unused type parameter" errors
                // Surprisingly, PhantomData<Fn(generics)> is Send even if
                // generics are not, unlike PhantomData<generics>
                _Phantom(Box<dyn Fn(#fn_params) -> () + Send>)
            }
            impl #ig Matcher #tg #wc {
                fn matches #lg (&self, #( #argnames: &#predty, )*) -> bool {
                    match self {
                        Matcher::Always => true,
                        Matcher::Func(__mockall_f) =>
                            __mockall_f(#(#argnames, )*),
                        Matcher::FuncST(__mockall_f) =>
                            (__mockall_f.get())(#(#argnames, )*),
                        Matcher::Pred(__mockall_pred) =>
                            [#pred_matches]
                            .iter()
                            .all(|__mockall_x| *__mockall_x),
                        _ => unreachable!()
                    }
                }
            }

            impl #ig Default for Matcher #tg #wc {
                #[allow(unused_variables)]
                fn default() -> Self {
                    Matcher::Always
                }
            }

            impl #ig ::std::fmt::Display for Matcher #tg #wc {
                fn fmt(&self, __mockall_fmt: &mut ::std::fmt::Formatter<'_>)
                    -> ::std::fmt::Result
                {
                    match self {
                        Matcher::Always => write!(__mockall_fmt, "<anything>"),
                        Matcher::Func(_) => write!(__mockall_fmt, "<function>"),
                        Matcher::FuncST(_) => write!(__mockall_fmt, "<single threaded function>"),
                        Matcher::Pred(__mockall_p) => {
                            write!(__mockall_fmt, #braces,
                                #(__mockall_p.#indices,)*)
                        }
                        _ => unreachable!(),
                    }
                }
            }
        ).to_tokens(tokens);
    }
}

struct Rfunc<'a> {
    f: &'a MockFunction
}

impl<'a> ToTokens for Rfunc<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let argnames = &self.f.argnames;
        let argty = &self.f.argty;
        let fn_params = &self.f.fn_params;
        let (ig, tg, wc) = self.f.egenerics.split_for_impl();
        let hrtb = self.f.hrtb();
        let lg = &self.f.alifetimes;
        let output = &self.f.output;
        quote!(
            enum Rfunc #ig #wc {
                Default,
                // Indicates that a `return_once` expectation has already
                // returned
                Expired,
                Mut(Box<dyn #hrtb FnMut(#(#argty, )*) -> #output + Send>),
                // Version of Rfunc::Mut for closures that aren't Send
                MutST(::mockall::Fragile<
                    Box<dyn #hrtb FnMut(#(#argty, )*) -> #output >>
                ),
                Once(Box<dyn #hrtb FnOnce(#(#argty, )*) -> #output + Send>),
                // Version of Rfunc::Once for closure that aren't Send
                OnceST(::mockall::Fragile<
                    Box<dyn #hrtb FnOnce(#(#argty, )*) -> #output>>
                ),
                // Prevent "unused type parameter" errors Surprisingly,
                // PhantomData<Fn(generics)> is Send even if generics are not,
                // unlike PhantomData<generics>
                _Phantom(Box<dyn Fn(#fn_params) -> () + Send>)
            }

            impl #ig  Rfunc #tg #wc {
                fn call_mut #lg (&mut self, #( #argnames: #argty, )* )
                    -> std::result::Result<#output, &'static str>
                {
                    match self {
                        Rfunc::Default => {
                            use ::mockall::ReturnDefault;
                            ::mockall::DefaultReturner::<#output>
                                ::return_default()
                        },
                        Rfunc::Expired => {
                            Err("called twice, but it returns by move")
                        },
                        Rfunc::Mut(__mockall_f) => {
                            Ok(__mockall_f( #(#argnames, )* ))
                        },
                        Rfunc::MutST(__mockall_f) => {
                            Ok((__mockall_f.get_mut())(#(#argnames,)*))
                        },
                        Rfunc::Once(_) => {
                            if let Rfunc::Once(mut __mockall_f) =
                                mem::replace(self, Rfunc::Expired) {
                                Ok(__mockall_f( #(#argnames, )* ))
                            } else {
                                unreachable!()
                            }
                        },
                        Rfunc::OnceST(_) => {
                            if let Rfunc::OnceST(mut __mockall_f) =
                                mem::replace(self, Rfunc::Expired) {
                                Ok((__mockall_f.into_inner())(#(#argnames,)*))
                            } else {
                                unreachable!()
                            }
                        },
                        Rfunc::_Phantom(_) => unreachable!()
                    }
                }
            }

            impl #ig std::default::Default for Rfunc #tg #wc
            {
                fn default() -> Self {
                    Rfunc::Default
                }
            }
        ).to_tokens(tokens);
    }
}

enum MockItemContent {
    Fn(MockFunction),
    Tokens(TokenStream)
}

pub(crate) struct MockItemModule {
    vis: Visibility,
    mock_ident: Ident,
    orig_ident: Option<Ident>,
    content: Vec<MockItemContent>
}

fn mock_fn(item_fn: ItemFn) -> ItemMod {
    let ident = format_ident!("__{}", item_fn.sig.ident);
    unimplemented!("5")
}

/// Generate a whole mod module from the mockable sort
impl From<MockableModule> for ItemMod {
    fn from(mod_: MockableModule) -> ItemMod {
        let attrs = Vec::new();
        let vis = mod_.vis;
        let mod_token = mod_.mod_token;
        let ident = mod_.mock_ident;
        let semi = None;
        let brace = token::Brace::default();

        let mut contents = Vec::<Item>::new();
        contents.push(Item::Verbatim(quote!(use super::T;)));
        for item in mod_.content.into_iter() {
            match item {
                Item::Fn(item_fn) => {
                    contents.push(
                        Item::Verbatim(quote!(#[allow(missing_docs)]))
                    );
                    contents.push(Item::Mod(mock_fn(item_fn)));
                },
                _ => {
                    compile_error(item.span(),
                        "Unsupported item type when mocking modules");
                }
            }
        }

        let content = Some((brace, contents));
        ItemMod {
            attrs,
            vis,
            mod_token,
            ident,
            content,
            semi
        }
    }
}

impl From<MockableModule> for MockItemModule {
    fn from(mod_: MockableModule) -> MockItemModule {
        let mock_ident = mod_.mock_ident.clone();
        let orig_ident = mod_.orig_ident;
        let content = mod_.content.into_iter().filter_map(|item| {
            let span = item.span();
            match item {
                Item::ExternCrate(_) | Item::Impl(_) =>
                {
                    // Ignore
                    None
                },
                Item::Static(is) => {
                    Some(MockItemContent::Tokens(is.into_token_stream()))
                },
                Item::Const(ic) => {
                    Some(MockItemContent::Tokens(ic.into_token_stream()))
                },
                Item::Fn(f) => {
                    Some(MockItemContent::Fn(
                        MockFunction::from((&mock_ident, f))
                    ))
                },
                Item::Mod(_) | Item::ForeignMod(_)
                    | Item::Struct(_) | Item::Enum(_)
                    | Item::Union(_) | Item::Trait(_) =>
                {
                    compile_error(span,
                        "Mockall does not yet support deriving nested mocks");
                    None
                },
                Item::Type(ty) => {
                    Some(MockItemContent::Tokens(ty.into_token_stream()))
                },
                Item::TraitAlias(ta) => {
                    Some(MockItemContent::Tokens(ta.into_token_stream()))
                },
                Item::Use(u) => {
                    Some(MockItemContent::Tokens(u.into_token_stream()))
                },
                _ => {
                    compile_error(span, "Unsupported item");
                    None
                }
            }
        }).collect::<Vec<_>>();

        MockItemModule {
            vis: mod_.vis,
            mock_ident: mod_.mock_ident,
            orig_ident,
            content
        }
    }
}

impl ToTokens for MockItemModule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut body = TokenStream::new();
        let mut cp_body = TokenStream::new();
        let modname = &self.mock_ident;

        for item in self.content.iter() {
            match item {
                MockItemContent::Tokens(ts) => ts.to_tokens(&mut body),
                MockItemContent::Fn(f) => {
                    //let mod_ident = format_ident!("__{}", &f.sig.ident);
                    //quote!(
                        //let __mockall_timeses = #mod_ident::EXPECTATIONS.lock()
                            //.unwrap()
                            //.checkpoint()
                            //.collect::<Vec<_>>();
                    //).to_tokens(&mut cp_body);
                    f.to_tokens(&mut body);
                },
            }
        }

        quote!(
            /// Verify that all current expectations for this function are
            /// satisfied and clear them.
            pub fn checkpoint() { #cp_body }
        ).to_tokens(&mut body);
        let docstr = {
            let inner_ds = if let Some(ident) = &self.orig_ident {
                format!("Mock version of the `{}` module", ident)
            } else {
                format!("TODO: write doc string")
            };
            quote!( #[doc = #inner_ds])
        };
        quote!(
            #docstr
            pub mod #modname { #body }
        ).to_tokens(tokens);
    }
}
