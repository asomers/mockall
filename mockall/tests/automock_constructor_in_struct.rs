// vim: tw=80
//! A struct with a constructor method
#![deny(warnings)]

use mockall::*;

#[allow(unused)]
struct A {}

#[allow(unused)]
#[automock]
impl A {
    fn new() -> Self {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let ctx = MockA::new_context();
    ctx.expect().returning(MockA::default);
    let _a: MockA = MockA::new();
}
