// vim: tw=80
#![deny(warnings)]

use mockall::*;
use std::sync::Mutex;

mock!{
    Foo {
        fn bar(x: u32) -> u64;
        // We must have a separate method for every should_panic test
        fn bar2(x: u32) -> u64;
        fn bar3(x: u32) -> u64;
    }
}

lazy_static! {
    static ref BAR_MTX: Mutex<()> = Mutex::new(());
}

// Checkpointing the mock object should not checkpoint static methods
#[test]
fn checkpoint() {
    let _m = BAR_MTX.lock().unwrap();

    let mut mock = MockFoo::new();
    let ctx = MockFoo::bar_context();
    ctx.expect()
        .returning(|_| 32)
        .times(1..3);
    mock.checkpoint();
    MockFoo::bar(0);
}

// It should also be possible to checkpoint just the context object
#[test]
#[should_panic(expected =
    "MockFoo::bar2: Expectation(<anything>) called fewer than 1 times")]
fn ctx_checkpoint() {
    let ctx = MockFoo::bar2_context();
    ctx.expect()
        .returning(|_| 32)
        .times(1..3);
    ctx.checkpoint();
    panic!("Shouldn't get here!");
}

// Expectations should be cleared when a context object drops
#[test]
#[should_panic(expected = "MockFoo::bar3: No matching expectation found")]
fn ctx_hygiene() {
    {
        let ctx0 = MockFoo::bar3_context();
        ctx0.expect()
            .returning(|x| u64::from(x + 1));
    }
    MockFoo::bar3(42);
}

#[test]
fn return_const() {
    let _m = BAR_MTX.lock().unwrap();

    let ctx = MockFoo::bar_context();
    ctx.expect()
        .return_const(42u64);
    assert_eq!(42, MockFoo::bar(41));
}

#[cfg_attr(not(feature = "nightly"), ignore)]
#[cfg_attr(not(feature = "nightly"), allow(unused_must_use))]
#[test]
fn return_default() {
    let _m = BAR_MTX.lock().unwrap();

    let ctx = MockFoo::bar_context();
    ctx.expect();
    let r = MockFoo::bar(5);
    assert_eq!(u64::default(), r);
}

#[test]
fn returning() {
    let _m = BAR_MTX.lock().unwrap();

    let ctx = MockFoo::bar_context();
    ctx.expect()
        .returning(|x| u64::from(x + 1));
    assert_eq!(42, MockFoo::bar(41));
}

#[test]
fn two_matches() {
    let _m = BAR_MTX.lock().unwrap();

    let ctx = MockFoo::bar_context();
    ctx.expect()
        .with(predicate::eq(42))
        .return_const(99u64);
    ctx.expect()
        .with(predicate::eq(69))
        .return_const(101u64);
    assert_eq!(101, MockFoo::bar(69));
    assert_eq!(99, MockFoo::bar(42));
}
