// vim: tw=80
//! A method that returns an immutable 'static reference
#![deny(warnings)]

use mockall::*;

#[automock]
trait A {
    fn foo(&self) -> &'static u32;
}

#[test]
fn return_const() {
    const X: u32 = 5;
    let mut mock = MockA::new();
    mock.expect_foo().return_const(&X);
    assert_eq!(5, *mock.foo());
}
