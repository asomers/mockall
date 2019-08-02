// vim: tw=80

use mockall::*;

#[automock(mod mock_ffi;)]
extern "Rust" {
    #[allow(unused)]
    fn foo(x: u32) -> i64;
}

#[test]
fn returning() {
    mock_ffi::expect_foo().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}
