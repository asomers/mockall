// vim: tw=80
#![deny(warnings)]

use mockall::*;

#[automock]
pub mod ffi {
    extern "C" {
        #[link_name = "foo__extern"]
        pub fn foo() -> u32;
    }
}

#[test]
fn return_const() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().return_const(42u32);
    assert_eq!(42, unsafe{mock_ffi::foo()});
}
