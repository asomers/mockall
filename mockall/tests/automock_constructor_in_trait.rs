// vim: tw=80
//! A trait with a constructor method

use mockall::*;

#[automock]
pub trait A {
    fn new() -> Self;
}

#[test]
fn returning() {
    MockA::expect_new().returning(|| MockA::default());
    let _a: MockA = <MockA as A>::new();
}
