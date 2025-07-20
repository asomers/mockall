// vim: tw=80
#![deny(warnings)]

use mockall::*;
use std::sync::Mutex;

#[automock]
mod ffi {
    extern "C" {
        // This is provided by the C library
        pub(super) fn isupper(c: i32) -> i32;
    }
}

static ISUPPER_MTX: Mutex<()> = Mutex::new(());

// Ensure we can still use the original mocked function
#[test]
pub fn normal_usage() {
    let _m = ISUPPER_MTX.lock();
    assert!(0 != unsafe { ffi::isupper('A' as i32)});    // 'A'
    assert_eq!(0, !unsafe { ffi::isupper('a' as i32)});   // 'a'
}

#[test]
#[should_panic(expected = "mock_ffi::isupper(5): No matching expectation found")]
fn with_no_matches() {
    let _m = ISUPPER_MTX.lock();
    let ctx = mock_ffi::isupper_context();
    ctx.expect()
        .with(predicate::eq(4))
        .returning(i32::from);
    unsafe{ mock_ffi::isupper(5) };
}

#[test]
fn returning() {
    let _m = ISUPPER_MTX.lock();
    let ctx = mock_ffi::isupper_context();
    ctx.expect().returning(i32::from);
    assert_eq!(42, unsafe{mock_ffi::isupper(42)});
}

/// Ensure that the mock function can be called from C by casting it to a C
/// function pointer.
#[test]
fn c_abi() {
    let _m = ISUPPER_MTX.lock();
    let ctx = mock_ffi::isupper_context();
    ctx.expect().returning(i32::from);
    let p: unsafe extern "C-unwind" fn(i32) -> i32 = mock_ffi::isupper;
    assert_eq!(42, unsafe{p(42)});
}
