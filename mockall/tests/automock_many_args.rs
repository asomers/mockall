// vim: tw=80
//! mockall should be able to mock methods with at least 16 arguments
#![allow(clippy::too_many_arguments)]    // Good job, Clippy!
#![allow(clippy::type_complexity)]
#![deny(warnings)]

use mockall::{automock, predicate::*};

#[automock]
trait ManyArgs {
    fn foo(&self, _a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
           _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
           _a12: u8, _a13: u8, _a14: u8, _a15: u8);
    fn bar(&self, _a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
           _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
           _a12: u8, _a13: u8, _a14: u8, _a15: u8) -> &u32;
    fn baz(&mut self, _a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
           _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
           _a12: u8, _a13: u8, _a14: u8, _a15: u8) -> &mut u32;
    fn bean(_a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
           _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
           _a12: u8, _a13: u8, _a14: u8, _a15: u8);
}

#[test]
#[should_panic(expected =
    "MockManyArgs::foo: Expectation(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true) called fewer than 1 times")]
fn not_yet_satisfied() {
    let mut mock = MockManyArgs::new();
    mock.expect_foo()
        .with(always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), )
        .times(1)
        .returning(|_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _|  ());
}

#[test]
fn returning() {
    let mut mock = MockManyArgs::new();
    mock.expect_foo()
        .returning(|_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _|  ());
    mock.foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#[test]
fn return_const() {
    let mut mock = MockManyArgs::new();
    mock.expect_bar()
        .return_const(42);
    mock.bar(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#[test]
fn return_var() {
    let mut mock = MockManyArgs::new();
    mock.expect_baz()
        .return_var(42);
    mock.baz(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#[test]
fn static_method_returning() {
    let ctx = MockManyArgs::bean_context();
    ctx.expect()
        .returning(|_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _|  ());
    MockManyArgs::bean(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#[test]
#[should_panic(expected =
    "MockManyArgs::foo: Expectation(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true) called more than 1 times")]
fn too_many() {
    let mut mock = MockManyArgs::new();
    mock.expect_foo()
        .with(always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), always(), )
        .times(1)
        .returning(|_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _|  ());
    mock.foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    mock.foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}


