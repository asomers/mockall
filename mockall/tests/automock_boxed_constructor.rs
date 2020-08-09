// vim: tw=80
//! A trait with a constructor method that returns Box<Self>
#![deny(warnings)]

use mockall::*;

#[automock]
pub trait A {
    fn new() -> Box<Self>;
}

#[test]
fn returning() {
    let ctx = MockA::new_context();
    ctx.expect().returning(|| Box::new(MockA::default()));
    let _a: Box<MockA> = <MockA as A>::new();
}
