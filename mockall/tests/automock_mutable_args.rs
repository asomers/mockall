// vim: tw=80
//! A method with mutable arguments

use mockall::*;

struct Foo {}

#[allow(unused)]
#[automock]
impl Foo {
    fn foo(&self, mut x: u32) {}
    fn bar(mut self) {}
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|_| ());
    mock.expect_bar()
        .returning(|| ());
    mock.foo(42);
    mock.bar();
}
