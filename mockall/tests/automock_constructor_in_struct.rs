// vim: tw=80
//! A struct with a constructor method
#![deny(warnings)]

use mockall::*;

pub struct A {}

#[automock]
#[allow(clippy::new_without_default)]
impl A {
    pub fn new() -> Self {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let ctx = MockA::new_context();
    ctx.expect().returning(MockA::default);
    let _a: MockA = MockA::new();
}
