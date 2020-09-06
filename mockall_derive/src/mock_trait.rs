// vim: tw=80
use quote::{ToTokens, format_ident, quote};
use syn::{
    *,
    spanned::Spanned
};

use crate::{
    mock_function::{self, MockFunction},
    compile_error
};

pub(crate) struct MockTrait {
    pub attrs: Vec<Attribute>,
    pub consts: Vec<ImplItemConst>,
    pub struct_generics: Generics,
    pub methods: Vec<MockFunction>,
    pub path: Path,
    structname: Ident,
    pub types: Vec<ImplItemType>
}

impl MockTrait {
    pub fn name(&self) -> &Ident {
        &self.path.segments.last().unwrap().ident
    }

    /// Create a new MockTrait
    ///
    /// # Arguments
    /// * `structname` - name of the struct that implements this trait
    /// * `struct_generics` - Generics of the parent structure
    /// * `impl_`  -    Mockable ItemImpl for a trait
    /// * `vis`     -   Visibility of the struct
    pub fn new(structname: &Ident,
               struct_generics: &Generics,
               impl_: ItemImpl,
               vis: &Visibility) -> Self
    {
        let mut consts = Vec::new();
        let mut methods = Vec::new();
        let mut types = Vec::new();
        let path = if let Some((_, path, _)) = impl_.trait_ {
            path
        } else {
            compile_error(impl_.span(), "impl block must implement a trait");
            Path::from(format_ident!("__mockall_invalid"))
        };
        let ident = &path.segments.last().unwrap().ident;

        for ii in impl_.items.into_iter() {
            match ii {
                ImplItem::Const(iic) => {
                    consts.push(iic);
                },
                ImplItem::Method(iim) => {
                    let mf = mock_function::Builder::new(&iim.sig, &vis)
                        .attrs(&iim.attrs)
                        .levels(2)
                        .call_levels(0)
                        .struct_(structname)
                        .struct_generics(struct_generics)
                        .trait_(ident)
                        .build();
                    methods.push(mf);
                },
                ImplItem::Type(iit) => {
                    types.push(iit);
                },
                _ => {
                    compile_error(ii.span(),
                    "This impl item is not yet supported by MockAll");
                }
            }
        }
        MockTrait {
            attrs: impl_.attrs,
            consts,
            struct_generics: struct_generics.clone(),
            methods,
            path,
            structname: structname.clone(),
            types
        }
    }

    /// Generate code for the trait implementation on the mock struct
    ///
    /// # Arguments
    ///
    /// * `modname`:    Name of the parent struct's private module
    // Supplying modname is an unfortunately hack.  Ideally MockTrait
    // wouldn't need to know that.
    pub fn trait_impl(&self, modname: &Ident) -> impl ToTokens {
        let attrs = &self.attrs;
        let (ig, tg, wc) = self.struct_generics.split_for_impl();
        let consts = &self.consts;
        let calls = self.methods.iter()
                .map(|meth| meth.call(Some(modname)))
                .collect::<Vec<_>>();
        let contexts = self.methods.iter()
            .filter(|meth| meth.is_static())
            .map(|meth| meth.context_fn(Some(modname)))
            .collect::<Vec<_>>();
        let expects = self.methods.iter()
            .filter(|meth| !meth.is_static())
            .map(|meth| meth.expect(&modname))
            .collect::<Vec<_>>();
        let path = &self.path;
        let structname = &self.structname;
        let types = &self.types;
        quote!(
            #(#attrs)*
            impl #ig #path for #structname #tg #wc {
                #(#consts)*
                #(#types)*
                #(#calls)*
            }
            impl #ig #structname #tg #wc {
                #(#expects)*
                #(#contexts)*
            }
        )
    }
}
