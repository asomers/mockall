// vim: tw=80
//! A method whose closure argument references an associated type of the
//! mocked trait.  Regression test for issue #662.
#![deny(warnings)]

use mockall::*;

mod types {
    pub trait SearchItem {}

    #[derive(Clone, Debug, PartialEq)]
    pub struct ConcreteItem(pub u32);

    impl SearchItem for ConcreteItem {}
}

#[automock(type Item = types::ConcreteItem;)]
pub trait FooSearch {
    type Item: types::SearchItem;

    // The bound from the issue, in a where clause
    fn remove_if<P>(&mut self, predicate: P) -> Vec<Self::Item>
    where
        P: Fn(&Self::Item) -> bool + 'static;

    // The same bound, inline on the generic parameter
    fn retain<P: Fn(&Self::Item) -> bool + 'static>(
        &mut self, predicate: P
    ) -> Vec<Self::Item>;

    // The same bound, with the fully qualified associated type
    fn keep_if<P>(&mut self, predicate: P) -> Vec<Self::Item>
    where
        P: Fn(&<Self as FooSearch>::Item) -> bool + 'static;
}

mod returning {
    use super::*;

    #[test]
    fn where_clause_bound() {
        let mut mock = MockFooSearch::new();
        mock.expect_remove_if()
            .returning(|predicate| {
                if predicate(&types::ConcreteItem(42)) {
                    vec![types::ConcreteItem(42)]
                } else {
                    Vec::new()
                }
            });
        assert_eq!(
            vec![types::ConcreteItem(42)],
            mock.remove_if(|item| item.0 == 42)
        );
    }

    #[test]
    fn inline_bound() {
        let mut mock = MockFooSearch::new();
        mock.expect_retain()
            .returning(|predicate| {
                if predicate(&types::ConcreteItem(0)) {
                    Vec::new()
                } else {
                    vec![types::ConcreteItem(7)]
                }
            });
        assert_eq!(
            vec![types::ConcreteItem(7)],
            mock.retain(|item| item.0 == 99)
        );
    }

    #[test]
    fn fully_qualified_bound() {
        let mut mock = MockFooSearch::new();
        mock.expect_keep_if()
            .returning(|predicate| {
                if predicate(&types::ConcreteItem(3)) {
                    vec![types::ConcreteItem(3)]
                } else {
                    Vec::new()
                }
            });
        assert_eq!(
            vec![types::ConcreteItem(3)],
            mock.keep_if(|item| item.0 == 3)
        );
    }
}
