// vim: tw=80
//! A struct with an async function
#![deny(warnings)]

use futures::executor::block_on;
use mockall::*;

mock! {
    pub Foo {
        async fn foo(&self) -> u32;
        async fn bar() -> u32;
        async fn baz<T: 'static>(&self, t: T) -> T;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(42u32);
    assert_eq!(block_on(mock.foo()), 42);
}

#[test]
fn static_method() {
    let ctx = MockFoo::bar_context();
    ctx.expect()
        .return_const(42u32);
    assert_eq!(block_on(MockFoo::bar()), 42);
}

#[test]
fn generic_method() {
    let mut mock = MockFoo::new();
    mock.expect_baz()
        .with(predicate::eq(69u32))
        .return_const(42u32);
    assert_eq!(block_on(mock.baz(69u32)), 42u32);
}
