// vim: tw=80
//! Expectations on static non-generic methods must not match functions calls
//! for different const generic values.
#![deny(warnings)]

use mockall::*;

#[automock]
trait A<const L: usize, const X: bool> {
    fn bar() -> [u8; L];
}

/// Note: There can only be one panicking test on a static function, because it
/// will leave the static context Mutex poisoned.
#[should_panic]
#[test]
fn wrong_const_generic_expectation() {
    let ctx = MockA::<1, true>::bar_context();
    ctx.expect().return_const([13]);

    // We expect a panic, because the context above is for a different const generic value.
    MockA::<1, false>::bar();
}
