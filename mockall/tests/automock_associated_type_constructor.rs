// vim: tw=80
/// A constructor that returns Self as an associated type of some other trait.
/// This is very useful when working with Futures.

use mockall::*;

pub trait Future {
    type Item;
    type Error;
}

#[allow(unused)]
pub struct Foo{}

#[automock]
impl Foo {
    pub fn open() -> impl Future<Item=Self, Error=i32> {
        struct Bar {}
        impl Future for Bar {
            type Item=Foo;
            type Error=i32;
        }
        Bar{}
    }
}

#[test]
fn returning() {
    MockFoo::expect_open().returning(|| {
        struct Baz {}
        impl Future for Baz {
            type Item=MockFoo;
            type Error=i32;
        }
        Box::new(Baz{})
    });
    let _a = MockFoo::open();
}
