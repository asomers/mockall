// vim: tw=80
//! It should be possible to use a custom Result type in the signature of a
//! mocked method.  Regression test for
//! https://github.com/asomers/mockall/issues/73
#![deny(warnings)]

use mockall::*;

pub type Result<T> = std::result::Result<T, String>;

pub struct MyStruct {}

#[automock]
impl MyStruct {
    pub fn ret_static(&self) -> Result<i32> { unimplemented!() }
    pub fn ret_ref(&self) -> &Result<i32> { unimplemented!() }
    pub fn ret_refmut(&mut self) -> &mut Result<i32> { unimplemented!() }
}

#[test]
fn ret_ref() {
    let mut s = MockMyStruct::new();
    s.expect_ret_ref()
        .return_const(Ok(42));
    assert_eq!(Ok(42), *s.ret_ref());
}

#[test]
fn ret_ref_mut() {
    let mut s = MockMyStruct::new();
    s.expect_ret_refmut()
        .return_var(Ok(42));
    assert_eq!(Ok(42), *s.ret_refmut());
}

#[test]
fn ret_static() {
    let mut s = MockMyStruct::new();
    s.expect_ret_static()
        .return_const(Ok(42));
    assert_eq!(Ok(42), s.ret_static());
}
