// vim: tw=80
//! generic methods with bounds on their generic parameters
#![deny(warnings)]

use mockall::*;
use std::fmt::Debug;

struct X<T: Debug>(T);

#[automock]
trait Foo {
    fn foo<T: Debug + 'static>(&self, x: X<T>);
}

#[test]
fn withf() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<u32>()
        .withf(|x| x.0 == 42u32)
        .return_const(());

    mock.foo(X(42u32));
}
