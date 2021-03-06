// vim: tw=80
//! generic structs
#![deny(warnings)]

use mockall::*;

#[allow(unused)]
struct GenericStruct<T, V> {
    t: T,
    v: V
}
#[automock]
impl<T: 'static, V: 'static> GenericStruct<T, V> {
    #[allow(unused)]
    fn foo(&self, _x: u32) -> i64 {
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
