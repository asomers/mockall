// vim: tw=80
use super::*;

use quote::ToTokens;

use crate::{
    mock_trait::MockTrait,
    mock_item_struct::Methods
};

#[allow(dead_code)] // TODO: remove me 
pub(crate) struct MockItemTraitImpl2 {
    attrs: Vec<Attribute>,
    /// Name of the overall module that holds all of the mock stuff
    modname: Ident,
    generics: Generics,
    name: Ident,
    trait_: MockTrait,
    /// Name of the field of this type in the parent's structure
    fieldname: Ident,
}

impl From<MockableTraitImpl> for MockItemTraitImpl2 {
    fn from(mockable: MockableTraitImpl) -> MockItemTraitImpl2 {
        let attrs = mockable.impl_.attrs.clone();
        let structname = &mockable.name;
        let modname = gen_mod_ident(&mockable.name, None);
        let generics = mockable.impl_.generics.clone();
        let vis = Visibility::Public(Token![pub](Span::call_site()));
        let trait_ = MockTrait::new(structname, &generics, mockable.impl_, &vis);
        let name = format_ident!("{}_{}", &mockable.name, trait_.ss_name());
        let fieldname = format_ident!("{}_expectations", trait_.ss_name());
        Self {
            attrs,
            fieldname,
            generics,
            modname,
            name,
            trait_
        }
    }
}

impl ToTokens for MockItemTraitImpl2 {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let modname = format_ident!("{}_{}", &self.modname,
                                   self.trait_.ss_name());
        let trait_impl = self.trait_.trait_impl(&modname);
        let substruct = crate::mock_item_struct::MockItemTraitImpl {
            attrs: self.trait_.attrs.clone(),
            generics: self.generics.clone(),
            fieldname: format_ident!("{}_expectations",
                                     self.trait_.ss_name()),
            methods: Methods(self.trait_.methods.clone()),
            modname,
            name: self.name.clone(),
        };

        quote!(
            #substruct
            #trait_impl
        ).to_tokens(tokens);
    }
}
