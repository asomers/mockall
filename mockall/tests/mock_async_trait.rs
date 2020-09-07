// vim: tw=80
//! An async trait, for use with Futures
#![deny(warnings)]

use async_trait::async_trait;
use futures::executor::block_on;
use mockall::*;

#[async_trait]
pub trait Foo {
    async fn foo(&self) -> u32;
}

mock! {
    pub Bar { }
    #[async_trait]
    impl Foo for Bar {
        async fn foo(&self) -> u32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::new();
    mock.expect_foo()
        .return_const(42u32);
    assert_eq!(block_on(mock.foo()), 42);
}
