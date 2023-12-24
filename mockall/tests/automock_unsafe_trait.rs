// vim: ts=80
#![deny(warnings)]

use mockall::*;

#[automock]
#[allow(clippy::missing_safety_doc)]
pub unsafe trait Foo {
    fn foo(&self) -> i32;
}

struct Baz{}

#[automock]
#[trait_impl(Foo)]
impl Baz {}
#[automock]
unsafe impl Foo for Baz {
    fn foo(&self) -> i32 {
        unimplemented!()
    }
}

#[test]
fn automock_trait() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(42);

    assert_eq!(42, mock.foo());
}

#[test]
fn automock_trait_impl() {
    let mut mock = MockBaz::new();
    mock.expect_foo()
        .return_const(42);

    assert_eq!(42, mock.foo());
}
