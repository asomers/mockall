// vim: tw=80
//! Check on mock object drop if method calls are satisfied
//! and panic if there are too few or too many calls
//!
//! https://github.com/asomers/mockall/issues/463
#![deny(warnings)]

use std::panic;

use mockall::*;

#[automock]
pub trait Foo {
    fn foo(&self);
}

#[test]
fn expectation_satisfied() {
    let mut foo_mock = MockFoo::default();
    foo_mock
        .expect_foo()
        .times(1)
        .returning(|| ());

    foo_mock.foo();
    foo_mock.checkpoint();
}

#[test]
#[should_panic(expected = "MockFoo::foo: Expectation(<anything>) called 0 time(s) which is fewer than expected 1")]
fn too_few_calls() {
    let mut foo_mock = MockFoo::default();
    foo_mock
        .expect_foo()
        .times(1)
        .returning(|| ());

    foo_mock.checkpoint();
}

#[test]
#[should_panic(expected = "MockFoo::foo: Expectation(<anything>) called 1 time(s) which is more than expected 0")]
fn too_many_calls() {
    let mut foo_mock = MockFoo::default();
    foo_mock
        .expect_foo()
        .times(0)
        .returning(|| ());

    let _ = panic::catch_unwind(|| {
        foo_mock.foo();
    });
    foo_mock.checkpoint();
}
