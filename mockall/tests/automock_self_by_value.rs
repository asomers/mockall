// vim: tw=80
//! A method that consumes self
#![deny(warnings)]

use mockall::*;

pub struct MethodByValue {}

#[automock]
impl MethodByValue {
    pub fn foo(self, _x: u32) -> i64 {0}
    #[allow(unused_mut)]
    pub fn bar(mut self) {}
}

#[test]
fn immutable() {
    let mut mock = MockMethodByValue::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
fn mutable() {
    let mut mock = MockMethodByValue::new();
    mock.expect_bar()
        .returning(|| ());
    mock.bar();
}
