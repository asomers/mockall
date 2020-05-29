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

/// Performs transformations on the method to make it mockable
fn mockable_method(mut meth: ImplItemMethod, name: &Ident, generics: &Generics)
    -> ImplItemMethod
{
    demutify(&mut meth.sig.inputs);
    deimplify(&mut meth.sig.output);
    if let ReturnType::Type(_, ty) = &mut meth.sig.output {
        deselfify(ty, name, generics);
    }
    meth
}

/// Performs transformations on the method to make it mockable
fn mockable_trait_method(
    meth: &mut TraitItemMethod,
    name: &Ident,
    generics: &Generics)
{
    demutify(&mut meth.sig.inputs);
    deimplify(&mut meth.sig.output);
    if let ReturnType::Type(_, ty) = &mut meth.sig.output {
        deselfify(ty, name, generics);
    }
}

/// Performs transformations on the trait to make it mockable
fn mockable_trait(mut trait_: ItemTrait, name: &Ident, generics: &Generics)
    -> ItemTrait
{
    for item in trait_.items.iter_mut() {
        if let TraitItem::Method(tim) = item {
            mockable_trait_method(tim, name, generics);
        }
    }
    trait_
}

/// Converts a TraitItemMethod into an ImplItemMethod
fn tim2iim(m: syn::TraitItemMethod, vis: &syn::Visibility)
    -> syn::ImplItemMethod
{
    let empty_block = Block {
        brace_token: token::Brace::default(),
        stmts: Vec::new()
    };
    syn::ImplItemMethod{
        attrs: m.attrs,
        vis: vis.clone(),
        defaultness: None,
        sig: m.sig,
        block: empty_block
    }
}


pub(crate) struct MockableStruct {
    pub attrs: Vec<Attribute>,
    pub generics: Generics,
    /// Inherent methods of the mockable struct
    pub methods: Vec<ImplItemMethod>,
    pub name: Ident,
    /// Name of the original struct.
    pub original_name: Ident,
    pub vis: Visibility,
    pub traits: Vec<ItemTrait>
}

impl From<(Attrs, ItemTrait)> for MockableStruct {
    fn from((attrs, item_trait): (Attrs, ItemTrait)) -> MockableStruct {
        let trait_ = attrs.substitute_trait(&item_trait);
        let attrs = trait_.attrs.clone();
        let vis = trait_.vis.clone();
        let name = gen_mock_ident(&trait_.ident);
        let generics = trait_.generics.clone();
        let original_name = trait_.ident.clone();
        let traits = vec![mockable_trait(trait_, &name, &generics)];
        MockableStruct {
            attrs,
            vis,
            name,
            generics,
            methods: Vec::new(),
            original_name,
            traits
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
                methods.push(mockable_method(meth, &name, &item_impl.generics));
            }
        }
        let pub_token = Token![pub](Span::call_site());
        let vis = Visibility::Public(VisPublic{pub_token});
        MockableStruct {
            attrs: item_impl.attrs.clone(),
            vis,
            name,
            generics: item_impl.generics,
            methods,
            original_name,
            traits: Vec::new()
        }
    }
}

impl Parse for MockableStruct {
    fn parse(input: ParseStream) -> syn::parse::Result<Self> {
        let attrs = input.call(syn::Attribute::parse_outer)?;
        let vis: syn::Visibility = input.parse()?;
        let original_name: syn::Ident = input.parse()?;
        let mut generics: syn::Generics = input.parse()?;
        let wc: Option<syn::WhereClause> = input.parse()?;
        generics.where_clause = wc;
        let name = gen_mock_ident(&original_name);

        let impl_content;
        let _brace_token = braced!(impl_content in input);
        let mut methods = Vec::new();
        while !impl_content.is_empty() {
            let method: syn::TraitItem = impl_content.parse()?;
            match method {
                syn::TraitItem::Method(mut meth) => {
                    mockable_trait_method(&mut meth, &name, &generics);
                    methods.push(tim2iim(meth, &vis))
                },
                _ => {
                    return Err(input.error("Unsupported in this context"));
                }
            }
        }

        let mut traits = Vec::new();
        while !input.is_empty() {
            let trait_: syn::ItemTrait = input.parse()?;
            traits.push(mockable_trait(trait_, &name, &generics));
        }

        Ok(
            MockableStruct {
                attrs: attrs,
                vis,
                name,
                generics,
                methods,
                original_name,
                traits
            }
        )
    }
}
