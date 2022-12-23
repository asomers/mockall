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
}

#[test]
fn drop_expectations_on_panic() {
    panic::catch_unwind(|| {
        let ctx = MockFoo::foo_context();
        ctx.expect()
            .times(1)
            .return_const(42);
        panic!("Panicking!");
    }).unwrap_err();

    // The previously set expectation should've been cleared during the panic,
    // so we must set a new one.
    let ctx = MockFoo::foo_context();
    ctx.expect()
        .times(1)
        .return_const(42);
    assert_eq!(42, MockFoo::foo());
}
