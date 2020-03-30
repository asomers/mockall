// vim: tw=80
use super::*;

use quote::ToTokens;

use crate::mockable_item::{MockableItem, MockableModule};

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

struct MockFunction{}

impl From<(&Ident, ItemFn)> for MockFunction {
    fn from((ident, f): (&Ident, ItemFn)) -> MockFunction {
        unimplemented!("3")
    }
}

impl ToTokens for MockFunction {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        unimplemented!("4")
    }
}

enum MockItemContent {
    Fn(MockFunction),
    Tokens(TokenStream)
}

pub(crate) struct MockItemModule {
    vis: Visibility,
    mock_ident: Ident,
    content: Vec<MockItemContent>
}

impl From<MockableModule> for MockItemModule {
    fn from(mod_: MockableModule) -> MockItemModule {
        let mock_ident = mod_.mock_ident.clone();
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
            let inner_ds = format!("TODO: write doc string");
            //let inner_ds = format!("Mock version of the `{}` module", mod_.ident);
            quote!( #[doc = #inner_ds])
        };
        quote!(
            #docstr
            pub mod #modname { #body }
        ).to_tokens(tokens);
    }
}
