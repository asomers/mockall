// vim: tw=80
use super::*;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

pub(crate) struct MockableStruct {
    pub generics: Generics,
    /// Inherent methods of the mockable struct
    pub methods: Vec<TraitItemMethod>,
    pub name: Ident,
    pub vis: Visibility,
}

impl From<(Attrs, ItemTrait)> for MockableStruct {
    fn from((attrs, item_trait): (Attrs, ItemTrait)) -> MockableStruct {
        let trait_ = attrs.substitute_trait(&item_trait);
        let mut methods = Vec::new();
        for item in item_trait.items.into_iter() {
            if let TraitItem::Method(meth) = item {
                methods.push(meth);
            }
        }
        MockableStruct {
            //attrs: item_trait.attrs.clone(),
            vis: item_trait.vis.clone(),
            name: gen_mock_ident(&item_trait.ident),
            generics: item_trait.generics,
            methods,
            //traits: vec![trait_]
        }
    }
}

impl From<ItemImpl> for MockableStruct {
    fn from(_s: ItemImpl) -> MockableStruct {
        unimplemented!("8");
    }
}

impl Parse for MockableStruct {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        unimplemented!("7");
    }
}
