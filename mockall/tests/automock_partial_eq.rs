// vim: tw=80
//! Mockall should deselfify `Self` types, even if they aren't named `self`.
use mockall::*;

pub struct Foo {}
#[automock]
#[trait_impl(PartialEq)]
impl Foo {
    pub fn compare(&self, _other: &Self) -> bool {
        unimplemented!()
    }
}
#[automock]
impl PartialEq for Foo {
    fn eq(&self, _other: &Self) -> bool {
        unimplemented!()
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
