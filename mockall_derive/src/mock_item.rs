// vim: tw=80
use super::*;

use crate::{
    mock_function::MockFunction,
    mockable_item::{MockableItem, MockableModule}
};

/// A Mock item
pub(crate) enum MockItem {
    Module(MockItemModule),
    Struct(MockItemStruct)
}

impl From<MockableItem> for MockItem {
    fn from(mockable: MockableItem) -> MockItem {
        match mockable {
            MockableItem::Struct(s) => MockItem::Struct(
                MockItemStruct::from(s)
            ),
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
            MockItem::Struct(s) => s.to_tokens(tokens)
        }
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
        contents.push(Item::Verbatim(quote!(use super::T;)));   // TODO
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
                        MockFunction::from((&mock_ident, None, 1, f.sig, f.vis))
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
                    f.to_tokens(&mut body);
                    f.checkpoint().to_tokens(&mut cp_body);
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
