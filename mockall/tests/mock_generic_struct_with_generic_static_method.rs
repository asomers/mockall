// vim: tw=80
//! A generic struct with a generic method on a different parameter
#![deny(warnings)]

use mockall::*;
use std::sync::Mutex;

mock! {
    Foo<T: 'static> {
        fn foo<Q: 'static>(t: T, q: Q) -> u64;
        // We must use a different method for every should_panic test, so the
        // shared mutex doesn't get poisoned.
        fn foo2<Q: 'static>(t: T, q: Q) -> u64;
        fn foo3<Q: 'static>(t: T, q: Q) -> u64;
    }
}

lazy_static! {
    static ref FOO_MTX: Mutex<()> = Mutex::new(());
}

// Checkpointing the mock object should not checkpoint static methods too
#[test]
fn checkpoint() {
    let _m = FOO_MTX.lock().unwrap();

    let mut mock = MockFoo::<u32>::new();
    let ctx = MockFoo::<u32>::foo_context();
    ctx.expect::<i16>()
        .returning(|_, _| 0)
        .times(1..3);
    mock.checkpoint();
    MockFoo::foo(42u32, 69i16);
}

// It should also be possible to checkpoint just the context object
#[test]
#[should_panic(expected =
    "MockFoo::foo2: Expectation(<anything>) called fewer than 1 times")]
fn ctx_checkpoint() {
    let ctx = MockFoo::<u32>::foo2_context();
    ctx.expect::<i16>()
        .returning(|_, _| 0)
        .times(1..3);
    ctx.checkpoint();
    panic!("Shouldn't get here!");
}

// Expectations should be cleared when a context object drops
#[test]
#[should_panic(expected = "MockFoo::foo3: No matching expectation found")]
fn ctx_hygiene() {
    {
        let ctx0 = MockFoo::<u32>::foo3_context();
        ctx0.expect::<i16>()
            .returning(|_, _| 0);
    }
    MockFoo::foo3(42, 69);
}

#[cfg_attr(not(feature = "nightly"), ignore)]
#[cfg_attr(not(feature = "nightly"), allow(unused_must_use))]
#[test]
fn return_default() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::<u32>::foo_context();
    ctx.expect::<i16>();
    MockFoo::foo(5u32, 6i16);
}

#[test]
fn returning() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::<u32>::foo_context();
    ctx.expect::<i16>()
        .returning(|_, _| 0);
    MockFoo::foo(41u32, 42i16);
}

#[test]
fn two_matches() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::<u32>::foo_context();
    ctx.expect::<i16>()
        .with(predicate::eq(42u32), predicate::eq(0i16))
        .return_const(99u64);
    ctx.expect::<i16>()
        .with(predicate::eq(69u32), predicate::eq(0i16))
        .return_const(101u64);
    assert_eq!(101, MockFoo::foo(69u32, 0i16));
    assert_eq!(99, MockFoo::foo(42u32, 0i16));
}

#[test]
fn with() {
    let _m = FOO_MTX.lock().unwrap();

    let ctx = MockFoo::<u32>::foo_context();
    ctx.expect::<i16>()
        .with(predicate::eq(42u32), predicate::eq(99i16))
        .returning(|_, _| 0);
    MockFoo::foo(42u32, 99i16);
}
