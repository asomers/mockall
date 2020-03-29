// vim: tw=80
use super::*;

use quote::ToTokens;

use crate::mockable_item::MockableItem;

pub(crate) enum MockItem {
}

impl From<MockableItem> for MockItem {
    fn from(mockable: MockableItem) -> MockItem {
        unimplemented!()
    }
}

impl ToTokens for MockItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        unimplemented!()
    }
}
