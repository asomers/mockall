// vim: tw=80
use super::*;

pub(crate) struct MockableTraitImpl {
    //pub attrs: Vec<Attribute>,
    //pub consts: Vec<ImplItemConst>,
    //pub generics: Generics,
    pub impl_: ItemImpl,
    pub name: Ident,
}

impl From<ItemImpl> for MockableTraitImpl {
    fn from(item_impl: ItemImpl) -> Self {
        assert!(item_impl.trait_.is_some());
        let name = match &*item_impl.self_ty {
            Type::Path(type_path) => {
                let n = find_ident_from_path(&type_path.path).0;
                gen_mock_ident(&n)
            },
            x => {
                compile_error(x.span(),
                    "mockall_derive only supports mocking traits and structs");
                Ident::new("", Span::call_site())
            }
        };
        let generics = item_impl.generics.clone();
        let impl_ = crate::mockable_struct::mockable_item_impl(item_impl, &name, &generics);
        MockableTraitImpl { impl_, name}
    }
}

