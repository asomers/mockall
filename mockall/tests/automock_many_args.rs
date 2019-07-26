// vim: tw=80
//! mockall should be able to mock methods with at least 16 arguments
#![allow(clippy::too_many_arguments)]    // Good job, Clippy!
#![allow(clippy::type_complexity)]

use mockall::*;

#[automock]
trait ManyArgs {
    fn foo(&self, _a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
           _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
           _a12: u8, _a13: u8, _a14: u8, _a15: u8);
}

#[test]
fn returning() {
    let mut mock = MockManyArgs::new();
    mock.expect_foo()
        .returning(|_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _|  ());
    mock.foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}
