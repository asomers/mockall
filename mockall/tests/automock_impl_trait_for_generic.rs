// vim: tw=80
//! A generic struct that implements a trait
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self, x: u32) -> i64;
}

pub struct SomeStruct<T> {
    _t: std::marker::PhantomData<T>
}

#[automock]
#[trait_impl(Foo)]
impl<T> SomeStruct<T> {}
#[automock]
impl<T> Foo for SomeStruct<T> {
    fn foo(&self, _x: u32) -> i64 {
        42
    }
}

#[test]
fn returning() {
    let mut mock = MockSomeStruct::<u32>::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}
