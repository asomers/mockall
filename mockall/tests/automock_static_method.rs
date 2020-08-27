// vim: tw=80
//! automocking a trait with a static method
#![deny(warnings)]

use mockall::*;

#[automock]
trait A {
    fn bar() -> u32;
}

#[test]
fn returning() {
    let ctx = MockA::bar_context();
    ctx.expect()
        .returning(|| 42);
    assert_eq!(42, MockA::bar());
}
