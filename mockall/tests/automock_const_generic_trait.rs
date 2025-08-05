// vim: tw=80
//! const generic traits
#![deny(warnings)]

use mockall::*;

#[automock]
trait A<const L: usize, const C: bool> {
    fn foo(&self, t: [u8; L]);
    fn bar(&self) -> [u8; L];
}

#[test]
fn generic_arguments() {
    let mut mock = MockA::<16usize, true>::new();
    mock.expect_foo()
        .with(mockall::predicate::eq([42_u8; 16usize]))
        .return_const(());
    mock.foo([42_u8; 16usize]);
}

#[test]
fn generic_return() {
    let mut mock = MockA::<1_usize, false>::new();
    mock.expect_bar().return_const([13_u8]);
    assert_eq!([13_u8], mock.bar());
}
