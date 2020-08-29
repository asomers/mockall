// vim: tw=80
//! An async trait, for use with Futures
#![deny(warnings)]

use async_trait::async_trait;
use futures::executor::block_on;
use mockall::*;

#[automock]
#[async_trait]
pub trait Foo {
    async fn foo(&self) -> u32;
    async fn bar() -> u32;
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
