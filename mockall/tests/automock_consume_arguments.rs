// vim: tw=80
#![deny(warnings)]

use mockall::*;

struct NonCopy{}

#[automock]
trait T {
    fn foo(&self, x: NonCopy);
}

#[test]
fn returning() {
    let mut mock = MockT::new();
    mock.expect_foo()
        .returning(|_x: NonCopy| ());
    mock.foo(NonCopy{});
}
