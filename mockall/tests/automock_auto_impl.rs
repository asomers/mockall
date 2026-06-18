// vim: tw=80
//! A trait that uses both `#[automock]` and `#[auto_impl]`.
//!
//! `#[auto_impl]` only applies to trait definitions, so `#[automock]` must not
//! copy it onto the generated mock.
#![deny(warnings)]

use auto_impl::auto_impl;
use mockall::*;

#[automock]
#[auto_impl(&, Box)]
pub trait Foo {
    fn foo(&self, x: u32) -> u32;
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}

// The mock is reachable through auto_impl's generated impl for Box.
#[test]
fn boxed() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    let boxed: Box<dyn Foo> = Box::new(mock);
    assert_eq!(5, boxed.foo(4));
}
