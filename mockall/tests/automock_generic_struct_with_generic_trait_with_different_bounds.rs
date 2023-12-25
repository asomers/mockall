// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo<T> {
    fn foo(&self, x: T) -> T;
}

struct Bar<T: 'static> {
    _t: std::marker::PhantomData<T>
}
#[automock]
#[trait_impl(Foo<T>)]
impl<T: 'static> Bar<T> {}
#[automock]
impl<T: 'static> Foo<T> for Bar<T> {
    fn foo(&self, _x: T) -> T {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let mut mock = MockBar::<u32>::new();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(5u32, mock.foo(5u32));
}
