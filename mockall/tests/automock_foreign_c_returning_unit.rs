// vim: tw=80
///! Ensure that foreign functions can return ()

use mockall::*;


#[automock(mod mock_ffi;)]
extern "C" {
    #[allow(unused)]
    fn foo();
}

#[test]
fn returning() {
    mock_ffi::expect_foo().returning(|| ());
    unsafe{mock_ffi::foo()};
}
