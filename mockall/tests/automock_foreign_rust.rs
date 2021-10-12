// vim: tw=80
#![deny(warnings)]

use mockall::*;

#[automock]
pub mod ffi {
    extern "Rust" {
        pub fn foo(_x: u32) -> i64;
    }
}

#[test]
fn returning() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}
