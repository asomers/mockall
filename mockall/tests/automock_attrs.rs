// vim: tw=80
//! Attributes are applied to the mock object, too.
#![allow(unused)]
#![deny(warnings)]

use mockall::*;

#[automock]
mod m {
    #[cfg(target_os = "multics")]
    pub fn bloob(x: DoesNotExist) -> i64 {unimplemented!()}
    #[cfg(not(target_os = "multics"))]
    pub fn blarg(x: i32) -> i64 {unimplemented!()}
}

#[test]
fn returning() {
    let ctx = mock_m::blarg_context();
    ctx.expect()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock_m::blarg(4));
}

pub struct A{}
#[automock]
impl A {
    // Neither A::foo nor MockA::foo should be defined
    #[cfg(target_os = "multics")] pub fn foo(&self, x: DoesNotExist) {}
    // Both A::bar and MockA::bar should be defined
    #[cfg(not(target_os = "multics"))] pub fn bar(&self, _x: i32) -> i32 {0}
}

#[automock]
mod ffi {
    extern "C" {
        // mock_ffi::baz should not be defined
        #[cfg(target_os = "multics")]
        pub fn baz(x: DoesNotExist) -> i64;
        // mock_ffi::bean should be defined
        #[cfg(not(target_os = "multics"))]
        pub fn bean(x: u32) -> i64;
    }
}

#[test]
fn method() {
    let mut mock = MockA::new();
    mock.expect_bar()
        .returning(|x| x);
    assert_eq!(4, mock.bar(4));
}

#[test]
fn foreign() {
    let ctx = mock_ffi::bean_context();
    ctx.expect().returning(i64::from);
    assert_eq!(42, unsafe{mock_ffi::bean(42)});
}
