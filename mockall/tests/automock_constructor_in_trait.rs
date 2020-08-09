// vim: tw=80
//! A trait with a constructor method
#![deny(warnings)]

use mockall::*;

#[automock]
pub trait A {
    fn new() -> Self;
}

#[test]
fn returning() {
    let ctx = MockA::new_context();
    ctx.expect().returning(MockA::default);
    let _a: MockA = <MockA as A>::new();
}
