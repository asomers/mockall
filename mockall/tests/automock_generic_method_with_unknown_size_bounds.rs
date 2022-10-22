// vim: tw=80
//! generic methods with bounds on their generic parameters
#![deny(warnings)]

use mockall::*;
use std::fmt::Debug;

struct X<T: Debug + ?Sized>(Box<T>);

#[automock]
trait Foo {
    fn foo<T: Debug + 'static + ?Sized>(&self, x: X<T>);
}

trait Bar: Debug {
    fn get(&self) -> u32;
}

#[derive(Debug)]
struct Foobar {
    value: u32
}

impl Bar for Foobar {
    fn get(&self) -> u32 {
        self.value
    }
}

#[test]
fn withf() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<dyn Bar>()
        .withf(|x| x.0.get() == 42)
        .return_const(());

    mock.foo::<dyn Bar>(X(Box::new(Foobar { value: 42 })));
}
