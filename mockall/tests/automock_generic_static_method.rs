// vim: tw=80
//! generic static methods with generic arguments

use mockall::*;

#[automock]
trait A {
    fn bar<T: 'static>(t: T) -> u32;
}

#[test]
fn returning() {
    MockA::expect_bar::<i16>()
        .returning(|_| 42);
    assert_eq!(42, MockA::bar(-1i16));
}
