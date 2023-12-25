// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Bar {
    fn baz(x: u32) -> u64;
}

pub struct Foo {}
#[automock]
#[trait_impl(Bar)]
impl Foo {}
#[automock]
impl Bar for Foo {
    fn baz(_x: u32) -> u64 {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let ctx = MockFoo::baz_context();
    ctx.expect()
        .returning(|x| u64::from(x + 1));
    assert_eq!(42, MockFoo::baz(41));
}
