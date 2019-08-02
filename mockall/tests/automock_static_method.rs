// vim: tw=80
//! automocking a trait with a static method

use mockall::*;

#[automock]
trait A {
    fn bar() -> u32;
}

#[test]
fn returning() {
    MockA::expect_bar()
        .returning(|| 42);
    assert_eq!(42, MockA::bar());
}
