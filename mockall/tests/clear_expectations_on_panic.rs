// vim: tw=80
//! Static methods' expectations should be dropped during panic.
//!
//! https://github.com/asomers/mockall/issues/442
#![deny(warnings)]

use std::panic;

use mockall::*;

#[automock]
pub trait Foo {
    fn foo() -> i32;
    fn bar() -> i32;
}

#[test]
fn too_few_calls() {
    panic::catch_unwind(|| {
        let ctx = MockFoo::foo_context();
        ctx.expect()
            .times(1)
            .return_const(42);
    }).unwrap_err();

    // The previously set expectation should've been cleared during the panic,
    // so we must set a new one.
    let ctx = MockFoo::foo_context();
    ctx.expect()
        .times(1)
        .return_const(42);
    assert_eq!(42, MockFoo::foo());
}

// We shouldn't panic during drop in this case.  Regression test for
// https://github.com/asomers/mockall/issues/491
#[should_panic(expected = "called `Result::unwrap()` on an `Err` value: PoisonError { .. }")]
#[test]
fn too_many_calls() {
    panic::catch_unwind(|| {
        let ctx = MockFoo::bar_context();
        ctx.expect()
            .times(0);
        MockFoo::bar();
    }).unwrap_err();

    // This line will panic with a PoisonError, at least until issue #515 is
    // complete.  
    let _ctx = MockFoo::bar_context();
}
