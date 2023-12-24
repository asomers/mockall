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
    fn from(mut item_impl: ItemImpl) -> Self {
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
        let impl_ = if let Some((bang, _path, _)) = &item_impl.trait_ {
            if bang.is_some() {
                compile_error(bang.span(), "Unsupported by automock");
            }

            // Substitute any associated types in this ItemImpl.
            // NB: this would not be necessary if the user always fully
            // qualified them, e.g. `<Self as MyTrait>::MyType`
            let mut attrs = Attrs::default();
            for item in item_impl.items.iter() {
                match item {
                    ImplItem::Const(_iic) =>
                        (),
                    ImplItem::Fn(_meth) =>
                        (),
                    ImplItem::Type(ty) => {
                        attrs.attrs.insert(ty.ident.clone(), ty.ty.clone());
                    },
                    x => compile_error(x.span(), "Unsupported by automock")
                }
            }
            attrs.substitute_item_impl(&mut item_impl);
            crate::mockable_struct::mockable_item_impl(item_impl, &name, &generics)
        } else {
            unreachable!();
        };
        MockableTraitImpl { impl_, name}
    }
}

