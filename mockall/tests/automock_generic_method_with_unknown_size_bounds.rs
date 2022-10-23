// vim: tw=80
//! generic methods with unknown size bounds on their generic parameters
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo {
    fn foo<T: 'static + ?Sized>(&self, input: Box<T>);
}

trait Bar {
    fn get(&self) -> u32;
}

struct Foobar {
    value: u32,
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
        .withf(|x| x.get() == 42)
        .return_const(());

    mock.foo::<dyn Bar>(Box::new(Foobar { value: 42 }));
}
