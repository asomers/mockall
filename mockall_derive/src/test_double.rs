use proc_macro2::{Span, TokenStream};
use quote::quote;

pub(crate) fn attribute_internal(metadata: &str, input: &str, output: &mut TokenStream) {
    if !metadata.is_empty() {
        let error_message =
            "Invalid input to #[test_double] - use it like #[test_double].";
        panic!(error_message)
    }

    // Generate the AST from the token stream we were given
    let item: syn::Item = syn::parse_str(input).expect("Failed to parse input");

    process_single_item(item, output);
}

fn process_single_item(
    item: syn::Item,
    output: &mut TokenStream,
) {
    match item {
        syn::Item::Use(mut use_original) => {
            // Make a copy of the original use statement
            let mut use_double = use_original.clone();

            modify_use_for_original(&mut use_original);
            modify_use_for_double(&mut use_double);

            // Add the result to the back of our list of output tokens
            output.extend::<TokenStream>(
                quote! {
                    #use_original
                    #use_double
                }
                    .into(),
            );
        }
        _ => panic!("Only use statements can be in the #[test_double] macro"),
    }
}

fn modify_use_for_original(use_original: &mut syn::ItemUse) {
    // Add `#[cfg(not(test))]` to our original use statement
    let not_test = quote! { (not(test)) };
    let cfg_not_test = syn::Attribute {
        pound_token: Default::default(),
        style: syn::AttrStyle::Outer,
        bracket_token: Default::default(),
        path: create_cfg_path(),
        tokens: not_test.into(),
    };
    use_original.attrs.push(cfg_not_test);
}

fn modify_use_for_double(use_double: &mut syn::ItemUse) {
    // Add `#[cfg(test)]` to our test double use statement
    let test = quote! { (test) };
    let cfg_not_test = syn::Attribute {
        pound_token: Default::default(),
        style: syn::AttrStyle::Outer,
        bracket_token: Default::default(),
        path: create_cfg_path(),
        tokens: test.into(),
    };
    use_double.attrs.push(cfg_not_test);

    modify_tree_for_double(&mut use_double.tree);
}

// Change the name of the item used for the double use statement.
fn modify_tree_for_double(use_tree: &mut syn::UseTree) {
    match use_tree {
        syn::UseTree::Path(use_path) => modify_tree_for_double(&mut use_path.tree),
        syn::UseTree::Group(use_group) => {
            for tree in use_group.items.iter_mut() {
                modify_tree_for_double(tree)
            }
        }
        syn::UseTree::Name(use_name) => {
            // Change the imported name and add an "as" also
            // `use blah::Bar` => `use blah::BarMock as Bar`
            let original_ident = use_name.ident.clone();
            let default_ident = crate::gen_mock_ident(&original_ident);

            let rename = syn::UseRename {
                ident: default_ident,
                as_token: syn::token::As(Span::call_site()),
                rename: original_ident,
            };
            *use_tree = syn::UseTree::Rename(rename);
        }
        syn::UseTree::Rename(use_rename) => {
            // Change the imported name
            // `use blah::Blah as Foo` => `use blah::BlahMock as Foo`
            let default_ident = crate::gen_mock_ident(&use_rename.ident);
            use_rename.ident = default_ident;
        }
        syn::UseTree::Glob(_) => panic!("test_double macros do not support * imports"),
    }
}

fn create_cfg_path() -> syn::Path {
    syn::Ident::new("cfg", Span::call_site()).into()
}
