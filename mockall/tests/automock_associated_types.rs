// vim: tw=80
//! automatic-style mocking with associated types
#![deny(warnings)]

use mockall::*;

#[automock(type T=u32;)]
trait A {
    type T: Clone;
    fn foo(&self, x: Self::T) -> Self::T;
}

#[test]
fn returning() {
    let mut mock = MockA::new();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(4, mock.foo(4));
}
