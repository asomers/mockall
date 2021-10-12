// vim: tw=80
#![deny(warnings)]

use mockall::*;

#[automock]
mod ffi {
    extern "C" {
        pub(super) fn foo(x: u32) -> i64;
        // Every should_panic method needs to operate on a separate method so it
        // doesn't poison other tests
        #[allow(dead_code)]
        pub(super) fn foo1(x: u32) -> i64;
    }
}

// Ensure we can still use the original mocked function
#[allow(dead_code)]
fn normal_usage() {
    unsafe {
        ffi::foo(42);
    }
}

#[test]
#[cfg_attr(feature = "nightly", should_panic(
        expected = "mock_ffi::foo1(5): No matching expectation found"
))]
#[cfg_attr(not(feature = "nightly"), should_panic(
        expected = "mock_ffi::foo1(?): No matching expectation found"
))]
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
