// vim: tw=80
//! If mocked object calls happen in subthread and expectations are not satisfied, the test should fail.
//!
//! https://github.com/asomers/mockall/issues/463
#![deny(warnings)]

use std::sync::Arc;
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
    let foo_mock = Arc::new(foo_mock);
    let mut foo_check = foo_mock.clone();

    let handle = std::thread::spawn(move || {
        foo_mock.foo();
    });
    let _ = handle.join();

    Arc::get_mut(&mut foo_check).unwrap().checkpoint();
}

#[test]
#[should_panic(expected = "MockFoo::foo: Expectation(<anything>) called 1 time(s) which is fewer than expected 2")]
fn too_few_calls() {
    let mut foo_mock = MockFoo::default();
    foo_mock
        .expect_foo()
        .times(2)
        .returning(|| ());
    let foo_mock = Arc::new(foo_mock);
    let mut foo_check = foo_mock.clone();

    let handle = std::thread::spawn(move || {
        foo_mock.foo();
    });
    let _ = handle.join();

    Arc::get_mut(&mut foo_check).unwrap().checkpoint();
}

#[test]
#[should_panic(expected = "MockFoo::foo: Expectation(<anything>) called 1 time(s) which is more than expected 0")]
fn too_many_calls() {
    let mut foo_mock = MockFoo::default();
    foo_mock
        .expect_foo()
        .times(0)
        .returning(|| ());
    let foo_mock = Arc::new(foo_mock);
    let mut foo_check = foo_mock.clone();

    let handle = std::thread::spawn(move || {
        foo_mock.foo();
    });
    let _ = handle.join();

    Arc::get_mut(&mut foo_check).unwrap().checkpoint();
}
