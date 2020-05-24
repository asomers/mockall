// vim: tw=80
use proc_macro2::Span;
use quote::{ToTokens, format_ident, quote};
use syn::*;

use crate::mock_function::{self, MockFunction};

pub(crate) struct MockTrait {
    pub attrs: Vec<Attribute>,
    pub generics: Generics,
    pub methods: Vec<MockFunction>,
    pub name: Ident,
    structname: Ident,
}

impl MockTrait {
    pub fn name(&self) -> &Ident {
        &self.name
    }

    /// Create a new MockTrait
    ///
    /// # Arguments
    /// * `structname` - name of the struct that implements this trait
    /// * `trait_`  -    Mockable ItemTrait
    /// * `vis`     -   Visibility of the struct
    pub fn new(structname: &Ident, trait_: ItemTrait, vis: &Visibility) -> Self {
        let mut methods = Vec::new();
        for ti in trait_.items.into_iter() {
            if let TraitItem::Method(tim) = ti {
                let mf = mock_function::Builder::new(&tim.sig, &vis)
                    .attrs(&tim.attrs)
                    .levels(2)
                    .call_levels(0)
                    .struct_(structname)
                    .trait_(&trait_.ident)
                    .build();
                methods.push(mf);
            }
        }
        MockTrait {
            attrs: trait_.attrs,
            generics: trait_.generics,
            methods,
            name: trait_.ident,
            structname: structname.clone()
        }
    }

    /// Generate code for the expect_ method
    ///
    /// # Arguments
    ///
    /// * `modname`:    Name of the parent struct's private module
    // Supplying modname is an unfortunately hack.  Ideally MockTrait
    // wouldn't need to know that.
    pub fn trait_impl(&self, modname: &Ident) -> impl ToTokens {
        let (ig, tg, wc) = self.generics.split_for_impl();
        let ss_modname = format_ident!("{}_{}", &modname, self.name);
        let calls = self.methods.iter()
                .map(|meth| meth.call(None))
                .collect::<Vec<_>>();
        let expects = self.methods.iter()
                .map(|meth| meth.expect(&ss_modname))
                .collect::<Vec<_>>();
        let name = &self.name;
        let structname = &self.structname;
        quote!(
            impl #ig #name for #structname #tg #wc {
                #(#calls)*
            }
            impl #ig #structname #tg #wc {
                #(#expects)*
            }
        )
    }
}
