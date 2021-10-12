// vim: tw=80
//! A method with mutable arguments
#![deny(warnings)]

use mockall::*;

pub struct Foo {}

#[automock]
impl Foo {
    pub fn foo(&self, mut _x: u32) {}
    pub fn bar(&mut self, _x: i32) -> i32 {0}
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
