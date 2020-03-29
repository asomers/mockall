// vim: tw=80
use super::*;

/// An item that's ready to be mocked.
///
/// It should be functionally identical or near-identical to the original item,
/// but with minor alterations that make it suitable for mocking, such as
/// altered lifetimes.
pub(crate) enum MockableItem {
    Module(MockableModule),
    Manual(ManualMock)
}

impl From<(Attrs, Item)> for MockableItem {
    fn from((attrs, item): (Attrs, Item)) -> MockableItem {
        match item {
            Item::Impl(item_impl) =>
                MockableItem::Manual(ManualMock::from(item_impl)),
            Item::ForeignMod(item_foreign_mod) =>
                MockableItem::Module(
                    MockableModule::from((attrs, item_foreign_mod))
                ),
            Item::Mod(item_mod) =>
                MockableItem::Module(MockableModule::from(item_mod)),
            Item::Trait(trait_) =>
                MockableItem::Manual(ManualMock::from((attrs, trait_))),
            _ => unimplemented!()
        }
    }
}

impl From<ManualMock> for MockableItem {
    fn from(mock: ManualMock) -> MockableItem {
        // TODO: stuff like deselfify
        MockableItem::Manual(mock)
    }
}

pub(crate) struct MockableModule {
    vis: Visibility,
    mock_ident: Ident,
    content: Vec<Item>
}

impl From<(Attrs, ItemForeignMod)> for MockableModule {
    fn from((attrs, foreign): (Attrs, ItemForeignMod)) -> MockableModule {
        let mock_ident = attrs.modname.expect(concat!(
            "module name is required when mocking foreign functions,",
            " like `#[automock(mod mock_ffi)]`"
        ));
        let vis = Visibility::Public(VisPublic{
            pub_token: <Token![pub]>::default()
        });
        let content = foreign.items.into_iter()
            .map(|foreign_item| {
                match foreign_item {
                    ForeignItem::Fn(f) => {
                        let span = f.sig.span();
                        let mut sig = f.sig;
                        // Foreign functions are always unsafe.  Mock foreign
                        // functions should be unsafe too, to prevent "warning:
                        // unused unsafe" messages.
                        sig.unsafety = Some(Token![unsafe](span));
                        let block = Box::new(Block {
                            brace_token: token::Brace::default(),
                            stmts: Vec::new()
                        });
                        Item::Fn(
                            ItemFn {
                                attrs: f.attrs,
                                vis: f.vis,
                                sig,
                                block
                            }
                        )
                    },
                    _ => {
                        compile_error(foreign_item.span(),
                            "Unsupported foreign item type"
                        );
                        Item::Verbatim(TokenStream::default())
                    }
                }
            }).collect::<Vec<_>>();
        MockableModule { vis, mock_ident, content }
    }
}

impl From<ItemMod> for MockableModule {
    fn from(module: ItemMod) -> MockableModule {
        let span = module.span();
        // TODO: in the future, consider mocking non-public modules
        let vis = Visibility::Public(VisPublic{
            pub_token: Token![pub](module.vis.span())
        });
        let mock_ident = format_ident!("mock_{}", module.ident);
        let ident = module.ident;
        let content = if let Some((_, content)) = module.content {
            content
        } else {
            compile_error(span,
            "automock can only mock inline modules, not modules from another file");
            Vec::new()
        };
        MockableModule { vis, mock_ident, content }
    }
}
