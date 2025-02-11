// vim: tw=80
//! An async trait, for use with Futures
#![deny(warnings)]

use futures::executor::block_on;
use mockall::*;

#[trait_variant::make(Send)]
pub trait Foo {
    async fn foo(&self) -> u32;
}

mock! {
    pub Bar { }
    #[trait_variant::make(Send)]
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
