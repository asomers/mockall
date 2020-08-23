// vim: tw=80
use super::*;
use syn::parse::{Parse, ParseStream};

/// Make any implicit lifetime parameters explicit
fn add_lifetime_parameters(sig: &mut Signature) {
    fn add_to_trait_object(generics: &mut Generics, var: &Pat, to: &mut TypeTraitObject) {
        let mut has_lifetime = false;
        for bound in to.bounds.iter() {
            if let TypeParamBound::Lifetime(_) = bound {
                has_lifetime = true;
            }
        }
        if ! has_lifetime {
            let arg_ident = match *var {
                Pat::Wild(_) => {
                    compile_error(var.span(),
                        "Mocked methods must have named arguments");
                    format_ident!("dont_care")
                },
                Pat::Ident(ref pat_ident) => {
                    if let Some(r) = &pat_ident.by_ref {
                        compile_error(r.span(),
                            "Mockall does not support by-reference argument bindings");
                    }
                    if let Some((_at, subpat)) = &pat_ident.subpat {
                        compile_error(subpat.span(),
                            "Mockall does not support subpattern bindings");
                    }
                    pat_ident.ident.clone()
                },
                _ => {
                    compile_error(var.span(),
                        "Unsupported argument type");
                    format_ident!("dont_care")
                }
            };
            let s = format!("'__mockall_{}", arg_ident);
            let span = Span::call_site();
            let lt = Lifetime::new(&s, span);
            to.bounds.push(TypeParamBound::Lifetime(lt.clone()));
            generics.lt_token.get_or_insert(Token![<](span));
            generics.gt_token.get_or_insert(Token![>](span));
            let gpl = GenericParam::Lifetime(LifetimeDef::new(lt));
            generics.params.push(gpl);
        }
    }

    fn add_to_type(generics: &mut Generics, var: &Pat, ty: &mut Type) {
        match ty {
            Type::Array(ta) => add_to_type(generics, var, ta.elem.as_mut()),
            Type::BareFn(_) => (),
            Type::ImplTrait(_) => (),
            Type::Path(_) => (),
            Type::Ptr(_) => (),
            Type::Reference(tr) => {
                match tr.elem.as_mut() {
                    Type::Paren(tp) => {
                        if let Type::TraitObject(to) = tp.elem.as_mut() {
                            add_to_trait_object(generics, var, to);
                        } else {
                            add_to_type(generics, var, tr.elem.as_mut());
                        }
                    },
                    Type::TraitObject(to) => {
                        add_to_trait_object(generics, var, to);
                        // We need to wrap it in a Paren.  Otherwise it won't be
                        // syntactically valid after we add a lifetime bound,
                        // due to a "ambiguous `+` in a type" error
                        *tr.elem = Type::Paren(TypeParen {
                            paren_token: token::Paren::default(),
                            elem: Box::new(Type::TraitObject(to.clone()))
                        });
                    },
                    _ => add_to_type(generics, var, tr.elem.as_mut()),
                }
            },
            Type::Slice(ts) => add_to_type(generics, var, ts.elem.as_mut()),
            Type::Tuple(tt) => {
                for mut ty in tt.elems.iter_mut() {
                    add_to_type(generics, var, &mut ty)
                }
            },
            _ => compile_error(ty.span(), "unsupported type in this position")
        }
    }

    for arg in sig.inputs.iter_mut() {
        if let FnArg::Typed(pt) = arg {
            add_to_type(&mut sig.generics, &pt.pat, &mut pt.ty)
        }
    }
}

/// Filter a generics list, keeping only the elements specified by path_args
/// e.g. filter_generics(<A: Copy, B: Clone>, <A>) -> <A: Copy>
fn filter_generics(g: &Generics, path_args: &PathArguments)
    -> Generics
{
    let mut params = Punctuated::new();
    match path_args {
        PathArguments::None => ()/* No generics selected */,
        PathArguments::Parenthesized(p) => {
            compile_error(p.span(),
                          "Mockall does not support mocking Fn objects");
        },
        PathArguments::AngleBracketed(abga) => {
            let args = &abga.args;
            if g.where_clause.is_some() {
                compile_error(g.where_clause.span(),
                    "Mockall does not yet support where clauses here");
                return g.clone();
            }
            for param in g.params.iter() {
                match param {
                    GenericParam::Type(tp) => {
                        let matches = |ga: &GenericArgument| {
                            if let GenericArgument::Type(
                                Type::Path(type_path)) = ga
                            {
                                type_path.path.is_ident(&tp.ident)
                            } else {
                                false
                            }
                        };
                        if args.iter().any(matches) {
                            params.push(param.clone())
                        }
                    },
                    GenericParam::Lifetime(ld) => {
                        let matches = |ga: &GenericArgument| {
                            if let GenericArgument::Lifetime(lt) = ga {
                                *lt == ld.lifetime
                            } else {
                                false
                            }
                        };
                        if args.iter().any(matches) {
                            params.push(param.clone())
                        }
                    },
                    GenericParam::Const(_) => ()/* Ignore */,
                }
            }
        }
    };
    if params.is_empty() {
        Generics::default()
    } else {
        Generics {
            lt_token: Some(Token![<](g.span())),
            params,
            gt_token: Some(Token![>](g.span())),
            where_clause: None
        }
    }
}

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
    add_lifetime_parameters(&mut meth.sig);
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
    add_lifetime_parameters(&mut meth.sig);
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
        let traits = vec![mockable_trait(trait_, &name, &generics)];
        MockableStruct {
            attrs,
            vis,
            name,
            generics,
            methods: Vec::new(),
            traits
        }
    }
}

impl From<ItemImpl> for MockableStruct {
    fn from(item_impl: ItemImpl) -> MockableStruct {
        let name = match *item_impl.self_ty {
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
        let mut methods = Vec::new();
        let pub_token = Token![pub](Span::call_site());
        let vis = Visibility::Public(VisPublic{pub_token});
        let mut traits = Vec::new();
        if let Some((bang, path, _)) = item_impl.trait_ {
            if bang.is_some() {
                compile_error(bang.span(), "Unsupported by automock");
            }

            // Regenerate the trait that this block is implementing
            let generics = &item_impl.generics;
            let traitname = &path.segments.last().unwrap().ident;
            let trait_path_args = &path.segments.last().unwrap().arguments;
            let mut items = Vec::new();
            let mut attrs = Attrs::default();
            for item in item_impl.items.into_iter() {
                match item {
                    ImplItem::Method(meth) =>
                        items.push(
                            TraitItem::Method(TraitItemMethod {
                                attrs: meth.attrs,
                                sig: meth.sig,
                                default: None,
                                semi_token: Some(Token![;](Span::call_site()))
                            })
                        ),
                    ImplItem::Type(ty) => {
                        attrs.attrs.insert(ty.ident.clone(), ty.ty.clone());
                        items.push(
                            TraitItem::Type(TraitItemType {
                                attrs: ty.attrs,
                                type_token: ty.type_token,
                                ident: ty.ident,
                                generics: ty.generics,
                                colon_token: None,
                                bounds: Punctuated::new(),
                                default: Some((ty.eq_token, ty.ty.clone())),
                                semi_token: ty.semi_token
                            })
                        );
                    },
                    x => compile_error(x.span(), "Unsupported by automock")
                }
            }
            let trait_ = ItemTrait {
                attrs: item_impl.attrs.clone(),
                vis: vis.clone(),
                unsafety: item_impl.unsafety,
                auto_token: None,
                trait_token: Token![trait](Span::call_site()),
                ident: traitname.clone(),
                generics: filter_generics(&item_impl.generics, trait_path_args),
                colon_token: None,
                supertraits: Punctuated::new(),
                brace_token: token::Brace::default(),
                items
            };
            let trait_ = attrs.substitute_trait(&trait_);
            traits.push(mockable_trait(trait_, traitname, &generics));
        } else {
            for item in item_impl.items.into_iter() {
                if let ImplItem::Method(meth) = item {
                    methods.push(
                        mockable_method(meth, &name, &item_impl.generics)
                    );
                }
            }
        };
        MockableStruct {
            attrs: item_impl.attrs,
            name,
            generics: item_impl.generics,
            methods,
            traits,
            vis
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
                attrs,
                vis,
                name,
                generics,
                methods,
                traits
            }
        )
    }
}

#[cfg(test)]
mod t {
    use super::*;

mod add_lifetime_parameters {
    use super::*;

    #[test]
    fn array() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: [&dyn T; 1]);
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo<'__mockall_x>(&self, x: [&(dyn T + '__mockall_x); 1]);)
                .to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn bare_fn_with_named_args() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: fn(&dyn T));
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo(&self, x: fn(&dyn T));).to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn plain() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: &dyn T);
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo<'__mockall_x>(&self, x: &(dyn T + '__mockall_x));)
                .to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn slice() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: &[&dyn T]);
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo<'__mockall_x>(&self, x: &[&(dyn T + '__mockall_x)]);)
                .to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn tuple() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: (&dyn T, u32));
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo<'__mockall_x>(&self, x: (&(dyn T + '__mockall_x), u32));)
                .to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn with_anonymous_lifetime() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: &(dyn T + '_));
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo(&self, x: &(dyn T + '_));).to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn with_parens() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: &(dyn T));
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo<'__mockall_x>(&self, x: &(dyn T + '__mockall_x));)
                .to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn with_lifetime_parameter() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo<'a>(&self, x: &(dyn T + 'a));
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo<'a>(&self, x: &(dyn T + 'a));).to_string(),
            quote!(#meth).to_string()
        );
    }

    #[test]
    fn with_static_lifetime() {
        let mut meth: TraitItemMethod = parse2(quote!(
            fn foo(&self, x: &(dyn T + 'static));
        )).unwrap();
        add_lifetime_parameters(&mut meth.sig);
        assert_eq!(
            quote!(fn foo(&self, x: &(dyn T + 'static));).to_string(),
            quote!(#meth).to_string()
        );
    }

    }
}
