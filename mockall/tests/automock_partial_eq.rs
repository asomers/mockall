// vim: tw=80
//! Mockall should deselfify `Self` types, even if they aren't named `self`.
use mockall::*;

mock! {
    #[derive(Debug)]
    pub Foo {
        fn compare(&self, other: &Self) -> bool;
    }
    impl PartialEq for Foo {
        fn eq(&self, other: &Self) -> bool;
    }
}

#[test]
fn inherent_method() {
    let mut x = MockFoo::default();
    let mut y = MockFoo::default();
    x.expect_compare()
        .return_const(true);
    y.expect_compare()
        .return_const(false);

    assert!(x.compare(&y));
    assert!(!y.compare(&x));
}

#[test]
fn trait_method() {
    let mut x = MockFoo::default();
    let mut y = MockFoo::default();
    x.expect_eq()
        .return_const(true);
    y.expect_eq()
        .return_const(false);

    assert_eq!(x, y);
    assert!(y != x);
}
