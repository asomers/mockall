// vim: tw=80
#![deny(warnings)]

use mockall::*;

#[automock]
mod ffi {
    extern "Rust" {
        #[allow(unused)]
        pub fn foo(x: u32) -> i64;
    }
}

#[test]
fn returning() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}
