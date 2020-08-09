// vim: tw=80
//! A generic method may have non-generic reference arguments
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn foo<T: 'static>(&self, t: T, x: &i16) -> u32;
    }
}

#[test]
fn with() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<i32>()
        .with(predicate::eq(4), predicate::eq(5))
        .return_const(42u32);
    assert_eq!(42, mock.foo(4i32, &5i16));
}
