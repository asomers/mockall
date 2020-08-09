// vim: tw=80
//! A method with mutable arguments
#![deny(warnings)]

use mockall::*;

struct Foo {}

#[allow(unused)]
#[automock]
impl Foo {
    fn foo(&self, mut x: u32) {}
    fn bar(&mut self, x: i32) -> i32 {0}
}

#[test]
fn mutable_self() {
    let mut mock = MockFoo::new();
    let mut count = 0;
    mock.expect_bar()
        .returning(move |x| {
            count += x;
            count
        });
    assert_eq!(5, mock.bar(5));
    assert_eq!(10, mock.bar(5));
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|_| ());
    mock.foo(42);
}
