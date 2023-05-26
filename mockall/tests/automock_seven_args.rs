// vim: tw=80
//! When mocking static functions just at the threshold of Clippy's type
//! complexity limit, no warnings should be emitted regarding the generated
//! code.
#![deny(warnings)]

use mockall::automock;

#[automock]
trait ManyArgs {
    fn foo(_a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8, _a6: u8);
}

#[test]
fn static_method_returning() {
    let ctx = MockManyArgs::foo_context();
    ctx.expect()
        .returning(|_, _, _, _, _, _, _|  ());
    MockManyArgs::foo(0, 0, 0, 0, 0, 0, 0);
}


