// vim: tw=80
//! A generic trait with a non-generic constructor method.
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo<T: 'static> {
    fn new(t: T) -> Self;
}

#[test]
fn return_once() {
    let mock = MockFoo::<u32>::default();

    let ctx = MockFoo::<u32>::new_context();
    ctx.expect()
        .return_once(move |_| mock);

    let _mock = MockFoo::new(5u32);
}
