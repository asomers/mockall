// vim: tw=80
//! Mockall should be able to mock #[must_use] methods.  The generated code
//! should contain #[must_use] on the mock method, but not the expect method.
#![deny(warnings)]

use mockall::*;

pub struct Foo {}

#[automock]
impl Foo {
    #[must_use]
    pub fn bloob(&self) -> i32 {unimplemented!()}
    #[must_use]
    pub fn blarg() -> i32 {unimplemented!()}
}

// test that basic code generation works with must_use structs and traits.  The
// exact output will be verified by the unit tests.
#[must_use]
pub struct MustUseStruct {}
#[automock]
impl MustUseStruct {}
#[automock]
#[must_use]
pub trait MustUseTrait {}

#[test]
fn must_use_method() {
    let mut mock = MockFoo::new();
    mock.expect_bloob()
        .return_const(42i32);
    assert_eq!(42, mock.bloob());
}

#[cfg(feature = "nightly")]
#[test]
fn may_not_use_expectation() {
    let mut mock = MockFoo::new();
    // This should not produce a "must_use" warning.
    mock.expect_bloob();
}

#[test]
fn must_use_static_method() {
    let ctx = MockFoo::blarg_context();
    ctx.expect()
        .return_const(42i32);
    assert_eq!(MockFoo::blarg(), 42);
}

#[cfg(feature = "nightly")]
#[test]
fn may_not_use_static_expectation() {
    let ctx = MockFoo::blarg_context();
    // This should not produce a "must_use" warning.
    ctx.expect();
}


