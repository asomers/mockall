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
    /// Names of the method arguments
    argnames: Vec<Pat>,
    /// Types of the method arguments
    argty: Vec<Type>,
    /// Type Generics of the Expectation object
    egenerics: Generics,
    /// Is this for a static method or free function?
    is_static: bool,
    /// The mockable version of the function
    item_fn: ItemFn,
    /// name of the function's parent module
    mod_ident: Ident,
    /// Expressions that create the predicate arguments from the call arguments
    predexprs: Vec<TokenStream>,
    /// Types used for Predicates.  Will be almost the same as args, but every
    /// type will be a non-reference type.
    predty: Vec<Type>,
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
        for fa in f.sig.inputs.iter() {
            if let FnArg::Typed(pt) = fa {
                let argname = (*pt.pat).clone();
                let aty = &pt.ty;
                //let aty = supersuperfy(&pt.ty, levels);
                if let Type::Reference(tr) = &**aty {
                    predexprs.push(quote!(#argname));
                    predty.push((*tr.elem).clone());
                } else {
                    predexprs.push(quote!(&#argname));
                    predty.push((**aty).clone());
                };
                argnames.push(argname);
                argty.push((**aty).clone());
            } else {
                is_static = false;
                ()    // Strip out the "&self" argument
            }
        }
        MockFunction {
            argnames,
            argty,
            is_static,
            predexprs,
            predty,
            egenerics,
            item_fn: f,
            mod_ident: ident.clone(),
        }
    }
}

impl ToTokens for MockFunction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let matcher = &Matcher{f: self};
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
                enum Rfunc {} // 42 lines
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
        let hrtb = Visibility::Inherited;   // TODO
        let refpredty = Visibility::Inherited; // TODO
        let preds = Visibility::Inherited; // TODO
        let predty = &self.f.predty;
        let fn_params = Visibility::Inherited; // TODO
        let argnames = &self.f.argnames;
        let pred_matches = Visibility::Inherited; // TODO
        let lg = Visibility::Inherited; // TODO
        let braces = Visibility::Inherited; // TODO
        let indices = [Visibility::Inherited]; // TODO
        quote!(
            enum Matcher #ig #wc {
                Always,
                Func(Box<dyn #hrtb Fn(#refpredty) -> bool + Send>),
                // Version of Matcher::Func for closures that aren't Send
                FuncST(::mockall::Fragile<Box<dyn #hrtb Fn(#refpredty) -> bool>>),
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
}enum MockItemContent {
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
