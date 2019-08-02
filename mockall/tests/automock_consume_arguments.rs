// vim: tw=80

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
