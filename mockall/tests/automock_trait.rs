// vim: tw=80
//! automocking a trait
#![deny(warnings)]

use mockall::*;

#[automock]
trait SimpleTrait {
    fn foo(&self, x: u32) -> u32;
}

#[test]
fn returning() {
    let mut mock = MockSimpleTrait::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}
