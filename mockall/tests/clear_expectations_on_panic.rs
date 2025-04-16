// vim: tw=80
//! Static methods' expectations should be dropped during panic.
//!
//! https://github.com/asomers/mockall/issues/442
#![deny(warnings)]

use std::{panic, sync::Mutex};

use mockall::*;

#[automock]
pub trait Foo {
    fn foo() -> i32;
    fn bar() -> i32;
}

static FOO_MTX: Mutex<()> = Mutex::new(());

#[test]
fn too_few_calls() {
    let _m = FOO_MTX.lock().unwrap();

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
#[test]
fn too_many_calls() {
    let _m = FOO_MTX.lock().unwrap();

    panic::catch_unwind(|| {
        let ctx = MockFoo::bar_context();
        ctx.expect()
            .times(0);
        MockFoo::bar();
    }).unwrap_err();

    let _ctx = MockFoo::bar_context();
}

// If we panic due to not setting any expectations at all, the mock method's
// mutex shouldn't be poisoned.
#[test]
fn no_expectations_at_all() {
    let _m = FOO_MTX.lock().unwrap();

    panic::catch_unwind(|| {
        MockFoo::bar();
    }).unwrap_err();

    let _ctx = MockFoo::bar_context();
}
