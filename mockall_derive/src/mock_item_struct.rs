// vim: tw=80
use super::*;

use quote::ToTokens;

use crate::mock_function::MockFunction;



pub(crate) struct MockItemStruct {
    generics: Generics,
    /// Inherent methods of the mock struct
    methods: Vec<MockFunction>,
    /// Name of the overall module that holds all of the mock stuff
    // TODO: base this on name so we can get rid of MockableStruct.original_name
    modname: Ident,
    name: Ident,
    //traits: Vec<MockTraitImpl>
    vis: Visibility,
}

impl From<MockableStruct> for MockItemStruct {
    fn from(mockable: MockableStruct) -> MockItemStruct {
        let mock_ident = gen_mod_ident(&mockable.name, None);
        let modname = gen_mod_ident(&mockable.original_name, None);
        let struct_name = &mockable.name;
        let vis = Visibility::Public(VisPublic{
            pub_token: Token![pub](Span::call_site())
        });
        let methods = mockable.methods.into_iter()
            .map(|meth|
                mock_function::Builder::new(&meth.sig, &vis)
                    .parent(&mock_ident)
                    .struct_(&struct_name)
                    .levels(2)
                    .build()
            ).collect::<Vec<_>>();
        MockItemStruct {
            generics: mockable.generics,
            methods,
            modname,
            name: mockable.name,
            //traits: Vec::new()
            vis: mockable.vis
        }
    }
}

impl ToTokens for MockItemStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let struct_name = &self.name;
        let (ig, tg, wc) = self.generics.split_for_impl(); //TODO
        let methods = &self.methods;
        let modname = &self.modname;
        let vis = &self.vis;
        let mut default_body = TokenStream::new();  // TODO
        quote!(
            #[allow(non_snake_case)]
            #[doc(hidden)]
            pub mod #modname {
                use super::*;
                #(#methods)*
                //mod_body
            }
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #[allow(missing_docs)]
            //attr_ts
            #vis struct #struct_name
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
