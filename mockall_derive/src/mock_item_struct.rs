// vim: tw=80
use super::*;

use quote::ToTokens;


pub(crate) struct MockItemStruct {
    generics: Generics,
    /// Inherent methods of the mock struct
    //methods: Vec<MockFunction>,
    name: Ident,
    //traits: Vec<MockTraitImpl>
}

impl From<MockableStruct> for MockItemStruct {
    fn from(mockable: MockableStruct) -> MockItemStruct {
        MockItemStruct {
            generics: mockable.generics,
            //methods: Vec::new(),
            name: mockable.name,
            //traits: Vec::new()
        }
    }
}

impl ToTokens for MockItemStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let struct_name = &self.name;
        let (ig, tg, wc) = self.generics.split_for_impl(); //TODO
        let mut default_body = TokenStream::new();  // TODO
        quote!(
            //#[allow(non_snake_case)]
            //#[doc(hidden)]
            //pub mod mod_ident {
                //use super::*;
                ////mod_body
            //}
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #[allow(missing_docs)]
            //attr_ts
            //vis
            struct #struct_name
            //ig wc
            {
                //body
            }
            impl #ig ::std::default::Default for #struct_name #tg #wc {
                fn default() -> Self {
                    Self {
                        #default_body
                    }
                }
            }
            impl #ig #struct_name #tg #wc {}
        ).to_tokens(tokens);
    }
}
