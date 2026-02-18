// vim: tw=80
use super::*;

/// Performs transformations on a function to make it mockable
fn mockable_fn(mut item_fn: ItemFn) -> ItemFn {
    demutify(&mut item_fn.sig.inputs);
    deimplify(&mut item_fn.sig.output);
    item_fn
}

/// Performs transformations on a Foreign Mod to make it mockable
fn mockable_foreign_mod(mut ifm: ItemForeignMod) -> ItemForeignMod {
    for item in &mut ifm.items {
        if let ForeignItem::Fn(ref mut f) = item {
            fix_elipses(&mut f.sig);
        }
    }
    ifm
}

/// Performs transformations on an Item to make it mockable
fn mockable_item(item: Item) -> Item {
    match item {
        Item::Fn(item_fn) => Item::Fn(mockable_fn(item_fn)),
        Item::ForeignMod(ifm) => Item::ForeignMod(mockable_foreign_mod(ifm)),
        x => x
    }
}

/// An item that's ready to be mocked.
///
/// It should be functionally identical or near-identical to the original item,
/// but with minor alterations that make it suitable for mocking, such as
/// altered lifetimes.
pub(crate) enum MockableItem {
    Module(MockableModule),
    Struct(Box<MockableStruct>)
}

impl From<(Attrs, Item)> for MockableItem {
    fn from((attrs, item): (Attrs, Item)) -> MockableItem {
        match item {
            Item::Impl(item_impl) => MockableStruct::from(item_impl).into(),
            Item::Mod(item_mod) => MockableItem::Module(MockableModule::from(item_mod)),
            Item::Trait(trait_) => MockableStruct::from((attrs, trait_)).into(),
            _ => panic!("automock does not support this item type"),
        }
    }
}

impl From<MockableStruct> for MockableItem {
    fn from(mock: MockableStruct) -> MockableItem {
        MockableItem::Struct(Box::new(mock))
    }
}

impl MockableItem {
    pub fn from_autospy(attrs: Attrs, item: Item) -> MockableItem {
        match item {
            Item::Impl(item_impl) => MockableStruct::from_impl_spy(item_impl).into(),
            Item::Trait(trait_) => MockableStruct::from_trait_spy(attrs, trait_).into(),
            _ => panic!("autospy only supports traits and struct impls"),
        }
    }
}

pub(crate) struct MockableModule {
    pub attrs: TokenStream,
    pub vis: Visibility,
    pub mock_ident: Ident,
    /// Ident of the original module, if any
    pub orig_ident: Option<Ident>,
    pub content: Vec<Item>
}

impl From<ItemMod> for MockableModule {
    fn from(mod_: ItemMod) -> MockableModule {
        let span = mod_.span();
        let vis = mod_.vis;
        let mock_ident = format_ident!("mock_{}", mod_.ident);
        let orig_ident = Some(mod_.ident);
        let content = if let Some((_, content)) = mod_.content {
            content.into_iter()
            .map(mockable_item)
            .collect()
        } else {
            compile_error(span,
            "automock can only mock inline modules, not modules from another file");
            Vec::new()
        };
        MockableModule {
            attrs: TokenStream::new(),
            vis,
            mock_ident,
            orig_ident,
            content
        }
    }
}
