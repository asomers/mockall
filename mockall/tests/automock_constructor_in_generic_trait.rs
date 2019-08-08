// vim: tw=80
//! A generic trait with a non-generic constructor method.

use mockall::*;

#[automock]
trait Foo<T: 'static> {
    fn new(t: T) -> Self;
}

#[test]
fn return_once() {
    let mock = MockFoo::<u32>::default();

    MockFoo::<u32>::expect_new()
        .return_once(move |_| mock);

    let _mock = MockFoo::new(5u32);
}
