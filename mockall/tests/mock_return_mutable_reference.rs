// vim: tw=80
//! A struct with a method that returns a mutable reference
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn foo(&mut self, i: u32) -> &mut u32;
    }
}

#[test]
#[cfg_attr(not(feature = "nightly"),
    should_panic(expected = "MockFoo::foo: Expectation(<anything>) Returning default values requires"))]
#[cfg_attr(not(feature = "nightly"), allow(unused_must_use))]
fn return_default() {
    let mut mock = MockFoo::new();
    mock.expect_foo();
    let r = mock.foo(0);
    assert_eq!(u32::default(), *r);
}

#[test]
fn return_var() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_var(5u32);
    {
        let r = mock.foo(0);
        assert_eq!(5, *r);
        *r = 6;
    }
    assert_eq!(6, *mock.foo(0));
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|_| 5u32);
    let r = mock.foo(0);
    assert_eq!(5, *r);
}
