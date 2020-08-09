// vim: tw=80
//! A struct that implements a trait
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self, x: u32) -> i64;
}

#[allow(unused)]
struct SomeStruct {}

#[automock]
impl Foo for SomeStruct {
    fn foo(&self, _x: u32) -> i64 {
        42
    }
}

#[test]
fn returning() {
    let mut mock = MockSomeStruct::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, <MockSomeStruct as Foo>::foo(&mock, 4));
}
