// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self, _x: u32) -> i64;
}

pub struct Bar {}
#[automock]
#[mockall::trait_impl(Foo)]
impl Bar {
    pub fn bar(&self, _x: i64) -> u32 {
        unimplemented!()
    }
}
#[automock]
impl Foo for Bar {
    fn foo(&self, _x: u32) -> i64 {
        unimplemented!()
    }
}


#[test]
fn inherent_method() {
    let mut mock = MockBar::new();
    mock.expect_bar()
        .return_const(6u32);
    assert_eq!(6u32, mock.bar(5i64));
}

#[test]
fn trait_method() {
    let mut mock = MockBar::new();
    mock.expect_foo()
        .return_const(6);
    assert_eq!(6, mock.foo(5));
}

