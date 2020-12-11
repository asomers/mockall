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
                          "Mockall does not support mocking Fn objects.  See https://github.com/asomers/mockall/issues/139");
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

/// Performs transformations on the ItemImpl to make it mockable
fn mockable_item_impl(mut impl_: ItemImpl, name: &Ident, generics: &Generics)
    -> ItemImpl
{
    for item in impl_.items.iter_mut() {
        if let ImplItem::Method(ref mut iim) = item {
            mockable_method(iim, name, generics);
        }
    }
    impl_
}

/// Performs transformations on the method to make it mockable
fn mockable_method(meth: &mut ImplItemMethod, name: &Ident, generics: &Generics)
{
    demutify(&mut meth.sig.inputs);
    add_lifetime_parameters(&mut meth.sig);
    deimplify(&mut meth.sig.output);
    if let ReturnType::Type(_, ty) = &mut meth.sig.output {
        deselfify(ty, name, generics);
        deanonymize(ty);
    }
    sanity_check_sig(&meth.sig);
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
        deanonymize(ty);
    }
    sanity_check_sig(&meth.sig);
}

/// Generates a mockable item impl from a trait method definition
fn mockable_trait(trait_: ItemTrait, name: &Ident, generics: &Generics)
    -> ItemImpl
{
    let items = trait_.items.into_iter()
    .map(|ti| {
        match ti {
            TraitItem::Method(mut tim) => {
                mockable_trait_method(&mut tim, name, generics);
                ImplItem::Method(tim2iim(tim, &Visibility::Inherited))
            },
            TraitItem::Const(tic) => {
                ImplItem::Const(tic2iic(tic, &Visibility::Inherited))
            },
            TraitItem::Type(tit) => {
                ImplItem::Type(tit2iit(tit, &Visibility::Inherited))
            },
            _ => {
                compile_error(ti.span(), "Unsupported in this context");
                ImplItem::Verbatim(TokenStream::new())
            }
        }
    }).collect::<Vec<_>>();
    let mut path = Path::from(trait_.ident);
    let (_, tg, _) = trait_.generics.split_for_impl();
    if let Ok(abga) = parse2::<AngleBracketedGenericArguments>(quote!(#tg)) {
        path.segments.last_mut().unwrap().arguments =
            PathArguments::AngleBracketed(abga);
    }
    ItemImpl {
        attrs: trait_.attrs,
        defaultness: None,
        unsafety: trait_.unsafety,
        impl_token: <Token![impl]>::default(),
        generics: trait_.generics,
        trait_: Some((None, path, <Token![for]>::default())),
        // For now, self_ty is unused
        self_ty: Box::new(Type::Verbatim(TokenStream::new())),
        brace_token: trait_.brace_token,
        items
    }
}

fn sanity_check_sig(sig: &Signature) {
    for arg in sig.inputs.iter() {
        if let FnArg::Typed(pt) = arg {
            if let Type::ImplTrait(it) = pt.ty.as_ref() {
                let bounds = &it.bounds;
                let s = format!(
                    "Mockall does not support \"impl trait\" in argument position.  Use \"T: {}\" instead",
                    quote!(#bounds)
                );
                compile_error(it.span(), &s);
            }
        }
    }
}

/// Converts a TraitItemConst into an ImplItemConst
fn tic2iic(tic: TraitItemConst, vis: &syn::Visibility) -> ImplItemConst {
    let span = tic.span();
    let (eq_token, expr) = tic.default.unwrap_or_else(|| {
        compile_error(span,
            "Mocked associated consts must have a default implementation");
        (<Token![=]>::default(), Expr::Verbatim(TokenStream::new()))
    });
    ImplItemConst {
        attrs: tic.attrs,
        vis: vis.clone(),
        defaultness: None,
        const_token: tic.const_token,
        ident: tic.ident,
        colon_token: tic.colon_token,
        ty: tic.ty,
        eq_token,
        expr,
        semi_token: tic.semi_token
    }
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

/// Converts a TraitItemType into an ImplItemType
fn tit2iit(tit: TraitItemType, vis: &Visibility) -> ImplItemType {
    let span = tit.span();
    let (eq_token, ty) = tit.default.unwrap_or_else(|| {
        compile_error(span,
            "associated types in mock! must be fully specified");
        (token::Eq::default(), Type::Verbatim(TokenStream::new()))
    });
    ImplItemType {
        attrs: tit.attrs,
        vis: vis.clone(),
        defaultness: None,
        type_token: tit.type_token,
        ident: tit.ident,
        generics: tit.generics,
        eq_token,
        ty,
        semi_token: tit.semi_token,
    }
}

pub(crate) struct MockableStruct {
    pub attrs: Vec<Attribute>,
    pub consts: Vec<ImplItemConst>,
    pub generics: Generics,
    /// Inherent methods of the mockable struct
    pub methods: Vec<ImplItemMethod>,
    pub name: Ident,
    pub vis: Visibility,
    pub impls: Vec<ItemImpl>
}

impl From<(Attrs, ItemTrait)> for MockableStruct {
    fn from((attrs, item_trait): (Attrs, ItemTrait)) -> MockableStruct {
        let trait_ = attrs.substitute_trait(&item_trait);
        let attrs = trait_.attrs.clone();
        let vis = trait_.vis.clone();
        let name = gen_mock_ident(&trait_.ident);
        let generics = trait_.generics.clone();
        let impls = vec![mockable_trait(trait_, &name, &generics)];
        MockableStruct {
            attrs,
            consts: Vec::new(),
            vis,
            name,
            generics,
            methods: Vec::new(),
            impls
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
        let mut consts = Vec::new();
        let mut methods = Vec::new();
        let pub_token = Token![pub](Span::call_site());
        let vis = Visibility::Public(VisPublic{pub_token});
        let mut impls = Vec::new();
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
                    ImplItem::Const(iic) =>
                        items.push(
                            TraitItem::Const(TraitItemConst {
                                attrs: iic.attrs,
                                const_token: iic.const_token,
                                ident: iic.ident,
                                colon_token: iic.colon_token,
                                ty: iic.ty,
                                default: Some((
                                    iic.eq_token,
                                    iic.expr
                                )),
                                semi_token: iic.semi_token
                            })
                        ),
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
            impls.push(mockable_trait(trait_, traitname, &generics));
        } else {
            for item in item_impl.items.into_iter() {
                match item {
                    ImplItem::Method(mut meth) => {
                        mockable_method(&mut meth, &name, &item_impl.generics);
                        methods.push(meth)
                    },
                    ImplItem::Const(iic) => consts.push(iic),
                    // Rust doesn't allow types in an inherent impl
                    x => compile_error(x.span(),
                        "Unsupported by Mockall in this context"),
                }
            }
        };
        MockableStruct {
            attrs: item_impl.attrs,
            consts,
            name,
            generics: item_impl.generics,
            methods,
            impls,
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
        let mut consts = Vec::new();
        let mut methods = Vec::new();
        while !impl_content.is_empty() {
            let item: ImplItem = impl_content.parse()?;
            match item {
                ImplItem::Method(mut iim) => {
                    mockable_method(&mut iim, &name, &generics);
                    methods.push(iim);
                },
                ImplItem::Const(iic) => consts.push(iic),
                _ => {
                    return Err(input.error("Unsupported in this context"));
                }
            }
        }

        let mut impls = Vec::new();
        while !input.is_empty() {
            let item: Item = input.parse()?;
            match item {
                Item::Trait(it) => {
                    let note = "Deprecated mock! syntax.  Instead of \"trait X\", write \"impl X for Y\".  See PR #205";
                    let mut impl_ = mockable_trait(it, &name, &generics);
                    impl_.attrs.push(Attribute {
                        pound_token: <token::Pound>::default(),
                        style: AttrStyle::Outer,
                        bracket_token: token::Bracket::default(),
                        path: Path::from(format_ident!("deprecated")),
                        tokens: quote!((since = "0.9.0", note = #note)),
                    });
                    impls.push(impl_)
                },
                Item::Impl(ii) =>
                    impls.push(mockable_item_impl(ii, &name, &generics)),
                _ => return Err(input.error("Unsupported in this context")),
            }
        }

        Ok(
            MockableStruct {
                attrs,
                consts,
                vis,
                name,
                generics,
                methods,
                impls
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

mod sanity_check_sig {
    use super::*;

    #[test]
    #[should_panic(expected = "Mockall does not support \"impl trait\" in argument position.  Use \"T: SomeTrait\" instead.")]
    fn impl_trait() {
        let meth: ImplItemMethod = parse2(quote!(
            fn foo(&self, x: impl SomeTrait);
        )).unwrap();
        sanity_check_sig(&meth.sig);
    }
}
}
