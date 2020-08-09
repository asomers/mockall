// vim: tw=80
//! A method that has both a reference and a nonreference argument
#![deny(warnings)]

use mockall::*;

mock!{
    Foo {
        fn foo(&self, i0: i32, i1: &u16) -> i32;
    }
}

#[test]
fn with() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .with(predicate::eq(42), predicate::eq(1))
        .returning(|x, y| x + i32::from(*y));
    let x = 42i32;
    let y = 1u16;
    assert_eq!(43i32, mock.foo(x, &y));
}

#[test]
fn withf() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .withf(|x, y| *x == i32::from(*y))
        .returning(|x, y| x + i32::from(*y));
    let x = 42i32;
    let y = 42u16;
    assert_eq!(84i32, mock.foo(x, &y));
}

