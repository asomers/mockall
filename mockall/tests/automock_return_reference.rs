// vim: tw=80
//! A method that returns an immutable reference

use mockall::*;

#[automock]
trait A {
    fn foo(&self) -> &u32;
}

#[test]
fn return_const() {
    let mut mock = MockA::new();
    mock.expect_foo().return_const(5);
    assert_eq!(5, *mock.foo());
}
