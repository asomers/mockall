// vim: tw=80
//! A struct implements a trait with associated types
#![deny(warnings)]

use mockall::*;

#[allow(unused)]
struct Foo {}
#[automock]
impl Iterator for Foo {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_next().returning(|| None);
    assert!(mock.next().is_none());
}
