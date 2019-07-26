// vim: tw=80
//! A method that returns ownership of a value, rather than returning by Copy

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
