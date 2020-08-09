// vim: tw=80
//! Ensure that foreign functions can return ()
#![deny(warnings)]

use mockall::*;


#[automock(mod mock_ffi;)]
extern "C" {
    #[allow(unused)]
    fn foo();
}

#[test]
fn returning() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(|| ());
    unsafe{mock_ffi::foo()};
}
