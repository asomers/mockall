// vim: tw=80
//! static non-generic methods of generic structs shouldn't require any special
//! treatment when mocking.
#![deny(warnings)]

use mockall::*;

#[automock]
trait A<const L: usize> {
    fn bar() -> [u8; L];
}

#[test]
fn generic_return() {
    let ctx = MockA::<1_usize>::bar_context();
    ctx.expect().return_const([13_u8]);
    assert_eq!([13_u8], MockA::<1_usize>::bar());
}
