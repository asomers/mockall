// vim: tw=80
//! A method that returns a mutable reference
#![deny(warnings)]

use mockall::*;

#[automock]
trait A {
    fn foo(&mut self) -> &mut u32;
}

#[test]
fn return_var() {
    let mut mock = MockA::new();
    mock.expect_foo().return_var(5);
    {
        let r = mock.foo();
        assert_eq!(5, *r);
        *r = 6;
    }
    assert_eq!(6, *mock.foo());
}
