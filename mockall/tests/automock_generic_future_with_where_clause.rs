// vim: tw=80
//! A generic mock object with a method that has only lifetime generic
//! parameters, and a where clause that bounds a generic type not used by the
//! method.
//!
//! Mockall must not emit the where clause for the method's Expectation.
#![deny(warnings)]
#![allow(clippy::needless_lifetimes)]

use mockall::*;

struct Foo<T, V>((T, V));
trait MyTrait {
    type Item;

    fn myfunc(&self, cx: &NonStatic) -> Self::Item;
}
#[allow(dead_code)]
pub struct NonStatic<'ns>(&'ns i32);

#[automock]
#[trait_impl(MyTrait)]
impl<T, V> Foo<T, V> {}
#[automock]
impl<T, V> MyTrait for Foo<T, V> where T: Clone {
    type Item = V;

    fn myfunc<'a>(&self, _cx: &NonStatic<'a>) -> V { unimplemented!() }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::<u32, u32>::new();
    let x = 5i32;
    let ns = NonStatic(&x);
    mock.expect_myfunc()
        .return_const(42u32);
    assert_eq!(42u32, mock.myfunc(&ns));
}
