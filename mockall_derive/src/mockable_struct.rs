// vim: tw=80
use super::*;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream};

fn find_ident_from_path(path: &Path) -> (Ident, PathArguments) {
        if path.segments.len() != 1 {
            compile_error(path.span(),
                "mockall_derive only supports structs defined in the current module");
            return (Ident::new("", path.span()), PathArguments::None);
        }
        let last_seg = path.segments.last().unwrap();
        (last_seg.ident.clone(), last_seg.arguments.clone())
}

pub(crate) struct MockableStruct {
    pub generics: Generics,
    /// Inherent methods of the mockable struct
    pub methods: Vec<ImplItemMethod>,
    pub name: Ident,
    /// Name of the original struct.
    pub original_name: Ident,
    pub vis: Visibility,
}

impl From<(Attrs, ItemTrait)> for MockableStruct {
    fn from((attrs, item_trait): (Attrs, ItemTrait)) -> MockableStruct {
        let trait_ = attrs.substitute_trait(&item_trait);
        let mut methods = Vec::new();
        let pub_token = Token![pub](Span::call_site());
        let empty_block = Block {
            brace_token: token::Brace::default(),
            stmts: Vec::new()
        };
        for item in item_trait.items.into_iter() {
            if let TraitItem::Method(meth) = item {
                let iim = ImplItemMethod {
                    attrs: meth.attrs,
                    block: empty_block.clone(),
                    defaultness: None,
                    sig: meth.sig,
                    vis: Visibility::Public(VisPublic{pub_token})
                };
                methods.push(iim);
            }
        }
        MockableStruct {
            //attrs: item_trait.attrs.clone(),
            vis: item_trait.vis.clone(),
            name: gen_mock_ident(&item_trait.ident),
            generics: item_trait.generics,
            methods,
            original_name: item_trait.ident
            //traits: vec![trait_]
        }
    }
}

impl From<ItemImpl> for MockableStruct {
    fn from(item_impl: ItemImpl) -> MockableStruct {
        let (name, original_name) = match *item_impl.self_ty {
            Type::Path(type_path) => {
                let n = find_ident_from_path(&type_path.path).0;
                (gen_mock_ident(&n), n)
            },
            x => {
                compile_error(x.span(),
                    "mockall_derive only supports mocking traits and structs");
                (Ident::new("", Span::call_site()),
                 Ident::new("", Span::call_site()))
            }
        };
        let mut methods = Vec::new();
        for item in item_impl.items.into_iter() {
            if let ImplItem::Method(meth) = item {
                methods.push(meth);
            }
        }
        let pub_token = Token![pub](Span::call_site());
        let vis = Visibility::Public(VisPublic{pub_token});
        MockableStruct {
            vis,
            name,
            generics: item_impl.generics,
            methods,
            original_name
        }
    }
}

impl Parse for MockableStruct {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        unimplemented!("7");
    }
}
