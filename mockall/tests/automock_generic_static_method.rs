// vim: tw=80
//! generic static methods with generic arguments
#![deny(warnings)]

use mockall::*;
use std::sync::Mutex;

lazy_static! {
    static ref A_MTX: Mutex<()> = Mutex::new(());
}

#[automock]
trait A {
    fn bar<T: 'static>(t: T) -> u32;
}

#[test]
fn returning() {
    let _m = A_MTX.lock().unwrap();

    let ctx = MockA::bar_context();
    ctx.expect::<i16>()
        .returning(|_| 42);
    assert_eq!(42, MockA::bar(-1i16));
}

#[test]
fn return_const() {
    let _m = A_MTX.lock().unwrap();

    let ctx = MockA::bar_context();
    ctx.expect::<i16>()
        .return_const(42u32);
    assert_eq!(42, MockA::bar(-1i16));
}
