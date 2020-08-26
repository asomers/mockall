// vim: tw=80
//! A struct with an async function
#![deny(warnings)]

use futures::executor::block_on;
use mockall::*;

mock! {
    pub Foo {
        async fn foo(&self) -> u32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(42u32);
    assert_eq!(block_on(mock.foo()), 42);
}
