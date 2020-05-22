// vim: tw=80
use super::*;

use quote::ToTokens;

use crate::{
    mock_function::MockFunction,
    mock_trait::MockTrait
};


pub(crate) struct MockItemStruct {
    attrs: Vec<Attribute>,
    generics: Generics,
    /// Inherent methods of the mock struct
    methods: Vec<MockFunction>,
    /// Name of the overall module that holds all of the mock stuff
    // TODO: base this on name so we can get rid of MockableStruct.original_name
    modname: Ident,
    name: Ident,
    /// Is this a whole MockStruct or just a substructure for a trait impl?
    substruct: bool,
    traits: Vec<MockTrait>,
    vis: Visibility,
}

impl MockItemStruct {
    fn new_method(&self) -> impl ToTokens {
        // TODO: don't add the new method if the struct already has one
        if self.substruct {
            TokenStream::new()
        } else {
            quote!(
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
            )
        }
    }
}

impl From<MockableStruct> for MockItemStruct {
    fn from(mockable: MockableStruct) -> MockItemStruct {
        let mock_ident = gen_mod_ident(&mockable.name, None);
        let modname = gen_mod_ident(&mockable.original_name, None);
        let struct_name = &mockable.name;
        let vis = mockable.vis;
        let methods = mockable.methods.into_iter()
            .map(|meth|
                mock_function::Builder::new(&meth.sig, &meth.vis)
                    .attrs(&meth.attrs)
                    .parent(&mock_ident)
                    .struct_(&struct_name)
                    .levels(1)
                    .call_levels(0)
                    .build()
            ).collect::<Vec<_>>();
        let structname = &mockable.name;
        let traits = mockable.traits.into_iter()
            .map(|t| MockTrait::new(structname, t, &vis))
            .collect();
        MockItemStruct {
            attrs: mockable.attrs,
            generics: mockable.generics,
            methods,
            modname,
            name: mockable.name,
            substruct: false,
            traits,
            vis
        }
    }
}

impl ToTokens for MockItemStruct {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attrs = &self.attrs;
        let struct_name = &self.name;
        let (ig, tg, wc) = self.generics.split_for_impl(); //TODO
        let modname = &self.modname;
        let calls = if self.substruct {
            Vec::new()
        } else {
            self.methods.iter()
                .map(|meth| meth.call())
                .collect::<Vec<_>>()
        };
        let method_checkpoints = self.methods.iter()
            .map(|meth| meth.checkpoint())
            .collect::<Vec<_>>();
        let default_constructions = self.methods.iter()
            .map(|meth| {
                let name = meth.name();
                let attrs = meth.format_attrs(false);
                let expectations_obj = &meth.expectations_obj();
                quote!(#attrs #name: #modname::#expectations_obj::default(),)
            }).collect::<Vec<_>>();
        let expectations_objects = self.methods.iter()
            .map(|meth| {
                let name = meth.name();
                let attrs = meth.format_attrs(false);
                let expectations_obj = &meth.expectations_obj();
                quote!(#attrs #name: #modname::#expectations_obj)
            }).collect::<Vec<_>>();
        let expects = if self.substruct {
            Vec::new()
        } else {
            self.methods.iter()
                .map(|meth| meth.expect(&modname))
                .collect::<Vec<_>>()
        };
        let new_method = self.new_method();
        let priv_mods = self.methods.iter()
            .map(|meth| meth.priv_module())
            .collect::<Vec<_>>();
        let substructs = self.traits.iter()
            .map(|trait_| {
                MockItemStruct {
                    attrs: trait_.attrs.clone(),
                    generics: trait_.generics.clone(),
                    methods: trait_.methods.clone(),
                    modname: format_ident!("{}_{}", &self.modname, trait_.name()),
                    name: format_ident!("{}_{}", &self.name, trait_.name()),
                    substruct: true,
                    traits: Vec::new(),
                    vis: Visibility::Inherited,
                }
            }).collect::<Vec<_>>();
        let substruct_expectations = self.traits.iter()
            .map(|trait_| format_ident!("{}_expectations", trait_.name()))
            .collect::<Vec<_>>();
        let substruct_type_names = substructs.iter()
            .map(|ss| ss.name.clone())
            .collect::<Vec<_>>();
        let trait_impls = self.traits.iter()
            .map(|ss| ss.trait_impl(&self.modname))
            .collect::<Vec<_>>();
        let vis = &self.vis;
        quote!(
            #[allow(non_snake_case)]
            #[doc(hidden)]
            pub mod #modname {
                use super::*;
                #(#priv_mods)*
            }
            #[allow(non_camel_case_types)]
            #[allow(non_snake_case)]
            #[allow(missing_docs)]
            #(#attrs)*
            #vis struct #struct_name #ig #wc
            {
                #(#expectations_objects),*
                #(#substruct_expectations: #substruct_type_names),*
            }
            impl #ig ::std::default::Default for #struct_name #tg #wc {
                fn default() -> Self {
                    Self {
                        // TODO: try removing the type names, and just use
                        // "default"
                        #(#substruct_expectations: #substruct_type_names::default()),*
                        #(#default_constructions)*
                    }
                }
            }
            #(#substructs)*
            impl #ig #struct_name #tg #wc {
                #(#calls #expects)*
                /// Validate that all current expectations for all methods have
                /// been satisfied, and discard them.
                pub fn checkpoint(&mut self) {
                    #(self.#substruct_expectations.checkpoint();)*
                    #(#method_checkpoints)*
                }
                #new_method
            }
            #(#trait_impls)*
        ).to_tokens(tokens);
    }
}
