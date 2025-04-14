// vim: tw=80
//! const generic struct
#![deny(warnings)]

use mockall::*;

pub struct GenericStruct<const L: usize, const C: bool> {
    pub l: [u8; L],
}

#[automock]
impl<const L: usize, const C: bool> GenericStruct<L, C> {
    pub fn foo(&self, _t: [u8; L]) {}
    pub fn bar(&self) -> [u8; L] { [0; L] }
}

#[test]
fn generic_arguments() {
    let mut mock = MockGenericStruct::<16usize, true>::new();
    mock.expect_foo()
        .with(mockall::predicate::eq([42_u8; 16usize]))
        .return_const(());
    mock.foo([42_u8; 16usize]);
}

#[test]
fn generic_return() {
    let mut mock = MockGenericStruct::<1_usize, false>::new();
    mock.expect_bar().return_const([13_u8]);
    assert_eq!([13_u8], mock.bar());
}
