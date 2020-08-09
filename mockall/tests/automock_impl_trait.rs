// vim: tw=80
//! A method that returns "impl Trait"
#![deny(warnings)]

use mockall::*;
use std::fmt::Debug;

#[allow(unused)]
pub struct Foo {}

#[automock]
impl Foo {
    #[allow(unused)]
    fn foo(&self) -> impl Debug + Send { unimplemented!()}
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo().returning(|| Box::new(4));
    format!("{:?}", mock.foo());
}
