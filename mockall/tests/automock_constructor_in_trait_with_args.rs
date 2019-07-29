// vim: tw=80
//! A struct with a constructor method named "new" that has arguments.
//! mockall should mock the provided method, and not autogenerate a 0-argument
//! "new" method.

use mockall::*;

#[automock]
trait Foo {
    fn new(x: u32) -> Self;
}

#[test]
fn return_once() {
    let mock = MockFoo::default();

    MockFoo::expect_new()
        .return_once(|_| mock);

    let _mock = MockFoo::new(5);
}
