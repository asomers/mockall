// vim: tw=80
//! A trait with a constructor method that returns Box<Self>

use mockall::*;

#[automock]
pub trait A {
    fn new() -> Box<Self>;
}

#[test]
fn returning() {
    MockA::expect_new().returning(|| Box::new(MockA::default()));
    let _a: Box<MockA> = <MockA as A>::new();
}
