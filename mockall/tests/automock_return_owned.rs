// vim: tw=80
//! A method that returns ownership of a value, rather than returning by Copy
#![deny(warnings)]

use mockall::*;

struct NonCopy {}

#[automock]
trait T {
    fn foo(&self) -> NonCopy;
}

#[test]
fn return_once() {
    let mut mock = MockT::new();
    let r = NonCopy{};
    mock.expect_foo()
        .return_once(|| r);
    mock.foo();
}

#[test]
#[should_panic(expected = "MockT::foo: Expectation(<anything>) called twice, but it returns by move")]
fn return_once_too_many_times() {
    let mut mock = MockT::new();
    let r = NonCopy{};
    mock.expect_foo()
        .return_once(|| r);
    mock.foo();
    mock.foo();
}
