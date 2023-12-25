// vim: ts=80
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self, x: u32) -> u32;
}

struct Bar<T: Copy + 'static> {
    _t: std::marker::PhantomData<T>
}
#[automock]
#[trait_impl(Foo)]
impl<T: Copy + 'static> Bar<T> {}
#[automock]
impl<T: Copy + 'static> Foo for Bar<T> {
    fn foo(&self, _x: u32) -> u32 {
        unimplemented!()
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::<u32>::new();
    mock.expect_foo()
        .return_const(43u32);
    assert_eq!(43, mock.foo(42));
}

