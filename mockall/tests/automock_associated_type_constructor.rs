// vim: tw=80
//! A constructor that returns Self as an associated type of some other trait.
//! This is very useful when working with Futures.
#![deny(warnings)]

use mockall::*;

pub trait MyIterator {
    type Item;
}

#[allow(unused)]
pub struct Foo{}

#[automock]
impl Foo {
    pub fn open() -> impl MyIterator<Item=Self> {
        struct Bar {}
        impl MyIterator for Bar {
            type Item=Foo;
        }
        Bar{}
    }
}

#[test]
fn returning() {
    let ctx = MockFoo::open_context();
    ctx.expect().returning(|| {
        struct Baz {}
        impl MyIterator for Baz {
            type Item = MockFoo;
        }
        Box::new(Baz{})
    });
    let _a = MockFoo::open();
}
