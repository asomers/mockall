// vim: tw=80
//! automocking a struct
#![deny(warnings)]

use std::panic;
use mockall::*;

pub struct SimpleStruct {}

#[automock]
impl SimpleStruct {
    pub fn foo(&self, _x: u32) -> i64 {
        42
    }
}

#[test]
fn returning() {
    let mut mock = MockSimpleStruct::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
fn clear() {
    let mut mock = MockSimpleStruct::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));

    mock.expect_foo().returning(|x| i64::from(x) + 2);
    assert_eq!(5, mock.foo(4));

    mock.clear_foo();

    mock.expect_foo().returning(|x| i64::from(x) + 2);
    assert_eq!(6, mock.foo(4));
}

#[test]
fn clear_and_expect() {
    let mut mock = MockSimpleStruct::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));

    mock.expect_foo().returning(|x| i64::from(x) + 2);
    assert_eq!(5, mock.foo(4));

    mock.clear_and_expect_foo().returning(|x| i64::from(x) + 2);
    assert_eq!(6, mock.foo(4));
}

#[test]
fn calling_foo_without_expectation_after_clear() {
    let mut mock = MockSimpleStruct::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));

    mock.clear_foo();
    let result = panic::catch_unwind(|| {
        mock.foo(4)
    });
    assert!(result.is_err());
}