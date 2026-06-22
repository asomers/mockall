// vim: tw=80
//! automocking a trait
#![deny(warnings)]

use std::panic;
use mockall::*;

#[automock]
trait SimpleTrait {
    fn foo(&self, x: u32) -> u32;
}

#[test]
fn returning() {
    let mut mock = MockSimpleTrait::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
fn clear() {
    let mut mock = MockSimpleTrait::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));

    mock.expect_foo().returning(|x| x + 2);
    assert_eq!(5, mock.foo(4));

    mock.clear_foo();

    mock.expect_foo().returning(|x| x + 2);
    assert_eq!(6, mock.foo(4));
}

#[test]
fn clear_and_expect() {
    let mut mock = MockSimpleTrait::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));

    mock.expect_foo().returning(|x| x + 2);
    assert_eq!(5, mock.foo(4));

    mock.clear_and_expect_foo().returning(|x| x + 2);
    assert_eq!(6, mock.foo(4));
}

#[test]
fn calling_foo_without_expectation_after_clear() {
    let mut mock = MockSimpleTrait::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));

    mock.clear_foo();
    let result = panic::catch_unwind(|| {
        mock.foo(4)
    });
    assert!(result.is_err());
}

