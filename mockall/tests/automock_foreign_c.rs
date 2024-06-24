// vim: tw=80
#![deny(warnings)]

use mockall::*;
use std::sync::Mutex;

#[automock]
mod ffi {
    extern "C" {
        pub(super) fn foo(x: u32) -> i64;
    }
}

static FOO_MTX: Mutex<()> = Mutex::new(());

// Ensure we can still use the original mocked function
pub fn normal_usage() {
    let _m = FOO_MTX.lock();
    unsafe {
        ffi::foo(42);
    }
}

#[test]
#[should_panic(expected = "mock_ffi::foo(5): No matching expectation found")]
fn with_no_matches() {
    let _m = FOO_MTX.lock();
    let ctx = mock_ffi::foo_context();
    ctx.expect()
        .with(predicate::eq(4))
        .returning(i64::from);
    unsafe{ mock_ffi::foo(5) };
}

#[test]
fn returning() {
    let _m = FOO_MTX.lock();
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}

/// Ensure that the mock function can be called from C by casting it to a C
/// function pointer.
#[test]
fn c_abi() {
    let _m = FOO_MTX.lock();
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    let p: unsafe extern "C-unwind" fn(u32) -> i64 = mock_ffi::foo;
    assert_eq!(42, unsafe{p(42)});
}
