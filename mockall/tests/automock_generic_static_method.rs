// vim: tw=80
//! generic static methods with generic arguments
#![allow(unused)]

use mockall::*;

#[automock]
trait A {
    fn bar<T: 'static>(t: T) -> u32;
}

#[test]
fn returning() {
    let ctx = MockA::bar_context();
    ctx.expect::<i16>()
        .returning(|_| 42);
    assert_eq!(42, MockA::bar(-1i16));
}
