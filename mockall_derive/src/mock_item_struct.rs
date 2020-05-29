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
    /// Does the original struct have a `new` method?
    has_new: bool,
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
        if self.has_new || self.substruct {
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
        let has_new = mockable.methods.iter()
            .any(|meth| meth.sig.ident == "new") ||
            mockable.traits.iter()
            .any(|trait_|
                trait_.items.iter()
                    .any(|ti| if let TraitItem::Method(tim) = ti {
                            tim.sig.ident == "new"
                        } else {
                            false
                        }
                    )
            );
        let methods = mockable.methods.into_iter()
            .map(|meth|
                mock_function::Builder::new(&meth.sig, &meth.vis)
                    .attrs(&meth.attrs)
                    .parent(&mock_ident)
                    .struct_(&struct_name)
                    .levels(2)
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
            has_new,
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
                .map(|meth| meth.call(Some(modname)))
                .collect::<Vec<_>>()
        };
        let method_checkpoints = self.methods.iter()
            .filter(|meth| !meth.is_static())
            .map(|meth| meth.checkpoint())
            .collect::<Vec<_>>();
        let default_constructions = self.methods.iter()
            .filter(|meth| !meth.is_static())
            .map(|meth| {
                let name = meth.name();
                let attrs = meth.format_attrs(false);
                let expectations_obj = &meth.expectations_obj();
                quote!(#attrs #name: #modname::#expectations_obj::default(),)
            }).collect::<Vec<_>>();
        let expectations_objects = self.methods.iter()
            .filter(|meth| !meth.is_static())
            .map(|meth| {
                let name = meth.name();
                let attrs = meth.format_attrs(false);
                let expectations_obj = &meth.expectations_obj();
                quote!(#attrs #name: #modname::#expectations_obj)
            }).collect::<Vec<_>>();
        let contexts = self.methods.iter()
            .map(|meth| if !meth.is_static() {
                // Only static methods have context methods, but we need
                // something here so contexts will have the same length as
                // calls.
                // TODO: after merging the 2020_refactor branch, eliminate
                // this hack by de-interleaving the calls, contexts, and
                // expects methods.
                Box::new(TokenStream::new()) as Box<dyn ToTokens>
            } else {
                Box::new(meth.context_fn(Some(modname))) as Box<dyn ToTokens>
            }).collect::<Vec<_>>();
        let expects = if self.substruct {
            Vec::new()
        } else {
            self.methods.iter()
                .map(|meth| if meth.is_static() {
                    // static methods' expect methods are generated by the
                    // Context objects, but we still need something here so
                    // expects will have the same length as calls.
                    // TODO: after merging the 2020_refactor branch, eliminate
                    // this hack by de-interleaving the calls and expects
                    // methods.
                    Box::new(TokenStream::new()) as Box<dyn ToTokens>
                } else {
                    Box::new(meth.expect(&modname)) as Box<dyn ToTokens>
                }).collect::<Vec<_>>()
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
                    // Never generate new methods for substructs
                    has_new: true,
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
            .map(|ss| {
                let modname = format_ident!("{}_{}", &self.modname, ss.name());
                ss.trait_impl(&modname)
            }).collect::<Vec<_>>();
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
                #(#calls #contexts #expects)*
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
