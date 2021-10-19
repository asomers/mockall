// vim: tw=80
//! automocking a struct
#![deny(warnings)]

use mockall::*;

pub struct SimpleStruct {}

#[automock]
impl SimpleStruct {
    pub fn foo(&self, _x: u32) -> i64 {
        42
    }
}

#[test]
fn returning() {
    let mut mock = MockSimpleStruct::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}
