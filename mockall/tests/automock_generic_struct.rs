// vim: tw=80
//! generic structs
#![deny(warnings)]

use mockall::*;

pub struct GenericStruct<T, V> {
    _t: T,
    _v: V
}
#[automock]
impl<T, V> GenericStruct<T, V> {
    pub fn foo(&self, _x: u32) -> i64 {
        42
    }
}

#[test]
fn returning() {
    let mut mock = MockGenericStruct::<u8, i8>::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}
