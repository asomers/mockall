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
        let vis = &mockable.vis;
        let methods = mockable.methods.into_iter()
            .map(|meth|
                mock_function::Builder::new(&meth.sig, &meth.vis)
                    .attrs(&meth.attrs)
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
        let modname = &self.modname;
        let calls = self.methods.iter()
            .map(|meth| meth.call())
            .collect::<Vec<_>>();
        let checkpoints = self.methods.iter()
            .map(|meth| meth.checkpoint())
            .collect::<Vec<_>>();
        let expects = self.methods.iter()
            .map(|meth| meth.expect(&modname))
            .collect::<Vec<_>>();
        let expectations_objects = self.methods.iter()
            .map(|meth| {
                let name = meth.name();
                let expectations_obj = &meth.expectations_obj();
                quote!(#name: #modname::#expectations_obj,)
            }).collect::<Vec<_>>();
        let priv_mods = self.methods.iter()
            .map(|meth| meth.priv_module())
            .collect::<Vec<_>>();
        let vis = &self.vis;
        let default_constructions = self.methods.iter()
            .map(|meth| {
                let name = meth.name();
                let expectations_obj = &meth.expectations_obj();
                quote!(#name: #modname::#expectations_obj::default(),)
            }).collect::<Vec<_>>();
        quote!(
            #[allow(non_snake_case)]
            #[doc(hidden)]
            pub mod #modname {
                use super::*;
                #(#priv_mods)*
                //mod_body
            }
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #[allow(missing_docs)]
            //attr_ts
            #vis struct #struct_name #ig #wc
            {
                #(#expectations_objects)*
            }
            impl #ig ::std::default::Default for #struct_name #tg #wc {
                fn default() -> Self {
                    Self {
                        #(#default_constructions)*
                    }
                }
            }
            impl #ig #struct_name #tg #wc {
                #(#calls #expects)*
                #[doc = "Immediately validate all expectations and clear them."]
                pub fn checkpoint(&mut self) {
                    #(#checkpoints)*
                }
                // TODO: don't add the new method if the struct already has one
                /// Create a new mock object with no expectations.
                ///
                /// This method will not be generated if the real struct
                /// already has a `new` method.  However, it *will* be
                /// generated if the struct implements a trait with a `new`
                /// method.  The trait's `new` method can still be called
                /// like `<MockX as TraitY>::new`
                pub fn new() -> Self {
                    Self::default()
                }
            }
        ).to_tokens(tokens);
    }
}
