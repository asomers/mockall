// vim: tw=80
//! Mockall should ignore and not emit attributes like "inline" that affect
//! code generation.
#![deny(warnings)]

use mockall::*;

pub struct Foo {}

#[automock]
impl Foo {
    #[inline]
    pub fn foo(&self) -> i32 {unimplemented!()}
    #[inline]
    pub fn bar() -> i32 {unimplemented!()}
    #[cold]
    pub fn baz(&self) -> i32 {unimplemented!()}
    #[cold]
    pub fn bean() -> i32 {unimplemented!()}
}

#[test]
fn inline_method() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(42i32);
    assert_eq!(mock.foo(), 42);
}

#[test]
fn inline_static_method() {
    let ctx = MockFoo::bar_context();
    ctx.expect()
        .return_const(42i32);
    assert_eq!(MockFoo::bar(), 42);
}

#[test]
fn cold_method() {
    let mut mock = MockFoo::new();
    mock.expect_baz()
        .return_const(42i32);
    assert_eq!(mock.baz(), 42);
}

#[test]
fn cold_static_method() {
    let ctx = MockFoo::bean_context();
    ctx.expect()
        .return_const(42i32);
    assert_eq!(MockFoo::bean(), 42);
}
