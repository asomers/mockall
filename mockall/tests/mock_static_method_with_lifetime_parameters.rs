// vim: tw=80
//! A static generic method whose only generic parameter is a lifetime parameter
#![deny(warnings)]

use mockall::*;
use std::sync::Mutex;

#[derive(Debug, Eq)]
struct X<'a>(&'a u32);

impl<'a> PartialEq for X<'a> {
    fn eq(&self, other: &X<'a>) -> bool {
        self.0 == other.0
    }
}

mock!{
    Foo {
        fn foo<'a>(x: &'a X<'a>) -> u32;
    }
}

lazy_static! {
    static ref FOO_MTX: Mutex<()> = Mutex::new(());
}

#[test]
fn return_const() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::foo_context();
    ctx.expect()
        .return_const(42u32);
    let x = X(&5);
    assert_eq!(42, MockFoo::foo(&x));
}

#[test]
fn returning() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::foo_context();
    ctx.expect()
        .returning(|f| *f.0);
    let x = X(&5);
    assert_eq!(5, MockFoo::foo(&x));
}

#[test]
fn withf() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::foo_context();
    ctx.expect()
        .withf(|f| *f.0 == 5)
        .return_const(42u32);
    let x = X(&5);
    assert_eq!(42, MockFoo::foo(&x));
}
