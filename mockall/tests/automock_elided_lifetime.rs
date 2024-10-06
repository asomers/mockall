// vim: tw=80
//! Mock a struct with a lifetime parameter
#![deny(warnings)]

use mockall::*;

pub struct NonStaticStruct<'nss> {
    _x: &'nss i32
}

#[automock]
impl NonStaticStruct<'_> {
    pub fn foo(&self) -> i64 {
        42
    }
}

#[test]
fn normal_method() {
    // This function serves to define a named lifetime
    fn has_lt<'a>(_x: &'a i8) -> MockNonStaticStruct<'a> {
        MockNonStaticStruct::<'a>::default()
    }

    let x = 42i8;
    let mut mock = has_lt(&x);
    mock.expect_foo()
        .returning(|| 5);
    assert_eq!(5, mock.foo());
}
