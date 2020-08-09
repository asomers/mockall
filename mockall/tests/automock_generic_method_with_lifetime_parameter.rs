// vim: tw=80
//! A generic method whose only generic parameter is a lifetime parameter is,
//! from Mockall's perspective, pretty much the same as a non-generic method.
#![deny(warnings)]

use mockall::*;

#[derive(Debug, Eq)]
struct X<'a>(&'a u32);

impl<'a> PartialEq for X<'a> {
    fn eq(&self, other: &X<'a>) -> bool {
        self.0 == other.0
    }
}

#[automock]
trait Foo {
    fn foo<'a>(&self, x: &'a X<'a>) -> u32;
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(42u32);
    let x = X(&5);
    assert_eq!(42, mock.foo(&x));
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|f| *f.0);
    let x = X(&5);
    assert_eq!(5, mock.foo(&x));
}

#[test]
fn withf() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .withf(|f| *f.0 == 5)
        .return_const(42u32);
    let x = X(&5);
    assert_eq!(42, mock.foo(&x));
}
