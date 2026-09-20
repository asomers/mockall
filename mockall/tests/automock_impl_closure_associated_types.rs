// vim: tw=80
//! A trait impl method whose closure argument references the impl's
//! associated type.  Regression test for issue #662.
#![deny(warnings)]

use mockall::*;

pub trait Search {
    type Item;

    fn remove_if<P>(&mut self, _predicate: P) -> Vec<Self::Item>
    where
        P: Fn(&Self::Item) -> bool + 'static;
}

pub struct Foo {}

#[automock]
impl Search for Foo {
    type Item = u32;

    fn remove_if<P>(&mut self, _predicate: P) -> Vec<Self::Item>
    where
        P: Fn(&Self::Item) -> bool + 'static,
    {
        unimplemented!()
    }
}

pub struct Bar<T> {
    _x: Option<T>
}

#[automock]
impl<T: Clone> Search for Bar<T> {
    type Item = T;

    fn remove_if<P>(&mut self, _predicate: P) -> Vec<Self::Item>
    where
        P: Fn(&Self::Item) -> bool + 'static,
    {
        unimplemented!()
    }
}

mod returning {
    use super::*;

    #[test]
    fn concrete() {
        let mut mock = MockFoo::new();
        mock.expect_remove_if()
            .returning(|predicate| {
                if predicate(&3) {
                    vec![3]
                } else {
                    Vec::new()
                }
            });
        assert_eq!(vec![3], mock.remove_if(|item| *item == 3));
    }

    #[test]
    fn generic() {
        let mut mock = MockBar::<u32>::new();
        mock.expect_remove_if()
            .returning(|predicate| {
                if predicate(&7) {
                    vec![7]
                } else {
                    Vec::new()
                }
            });
        assert_eq!(vec![7], mock.remove_if(|item| *item == 7));
    }
}
