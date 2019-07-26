// vim: tw=80
//! a method that returns impl Trait

use mockall::*;
use std::fmt::Debug;

mock!{
    Foo {
        fn foo(&self) -> impl Debug + Send;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo().returning(|| Box::new(4));
    format!("{:?}", mock.foo());
}
