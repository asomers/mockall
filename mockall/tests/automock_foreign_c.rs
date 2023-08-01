// vim: tw=80
#![deny(warnings)]

use mockall::*;
use std::ffi::{c_int, c_void};

#[automock]
mod ffi {
    use super::*;
    extern "C" {
        pub(super) fn foo(x: u32) -> i64;
        // Every should_panic method needs to operate on a separate method so it
        // doesn't poison other tests
        #[allow(dead_code)]
        pub(super) fn foo1(x: u32) -> i64;

        #[allow(dead_code)]
        pub fn cmpr(a: *const c_void, b: *const c_void) -> c_int;
    }
}

extern "C" {
    pub fn qsort(
        base: *mut c_void,
        num: usize,
        size: usize,
        compar: unsafe extern "C" fn(_: *const c_void, _: *const c_void) -> c_int
    );
}

// Ensure we can still use the original mocked function
pub fn normal_usage() {
    unsafe {
        ffi::foo(42);
    }
}

#[test]
#[should_panic(expected = "mock_ffi::foo1(5): No matching expectation found")]
fn with_no_matches() {
    let ctx = mock_ffi::foo1_context();
    ctx.expect()
        .with(predicate::eq(4))
        .returning(i64::from);
    unsafe{ mock_ffi::foo1(5) };
}

#[test]
fn returning() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}

/// Ensure that the mock function can be called from C by casting it to a C
/// function pointer.
#[test]
fn c_abi() {
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(i64::from);
    let p: unsafe extern "C" fn(u32) -> i64 = mock_ffi::foo;
    assert_eq!(42, unsafe{p(42)});
}

#[test]
fn c_abi_with_qsort() {
    let ctx = mock_ffi::cmpr_context();
    ctx.expect().returning(|x, y| 
        unsafe {
            (*(x as *const i32) > *(y as *const i32)) as i32
        }
    );
    let mut v = vec![3, 1, 4, 1];
    unsafe {
        qsort(v.as_mut_ptr() as *mut c_void, v.len(), std::mem::size_of::<i32>(), mock_ffi::cmpr);
    }
    assert_eq!(v, vec![1, 1, 3, 4]);
}