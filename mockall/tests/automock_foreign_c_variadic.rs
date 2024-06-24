// vim: tw=80
#![cfg_attr(feature = "nightly", feature(c_variadic))]
#![deny(warnings)]

#[cfg(feature = "nightly")]
use mockall::*;

#[automock]
#[cfg(feature = "nightly")]
pub mod ffi {
    extern "C" {
        pub fn foo(x: i32, y: i32, ...) -> i32;
    }
}

#[cfg(feature = "nightly")]
static FOO_MTX: std::sync::Mutex<()> = std::sync::Mutex::new(());

#[test]
#[cfg(feature = "nightly")]
#[cfg_attr(miri, ignore)]
fn mocked_c_variadic() {
    let _m = FOO_MTX.lock();
    let ctx = mock_ffi::foo_context();
    ctx.expect().returning(|x, y| x * y);
    assert_eq!(6, unsafe{mock_ffi::foo(2, 3, 1, 4, 1)});
}

#[test]
#[cfg(feature = "nightly")]
#[cfg_attr(miri, ignore)]
#[ignore = "https://github.com/rust-lang/rust/issues/126836"]
fn panics() {
    let _m = FOO_MTX.lock();
    unsafe{ mock_ffi::foo(1,2,3) };
}
