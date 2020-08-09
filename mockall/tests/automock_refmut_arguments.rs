// vim: tw=80
//! A method that takes mutable reference arguments, returning information
//! through its arguments like C functions often do.
#![deny(warnings)]

use mockall::*;

#[automock]
trait T {
    fn foo(&self, x: &mut u32);
}

#[test]
fn returning() {
    let mut mock = MockT::new();
    let mut x = 5;
    mock.expect_foo()
        .returning(|x: &mut u32| {
            *x = 42;
        });
    mock.foo(&mut x);
    assert_eq!(42, x);
}
