// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo<T: 'static> {
    fn foo(&self, x: T) -> T;
}

struct Bar<T: 'static, Z: 'static> {
    _t: std::marker::PhantomData<T>,
    _z: std::marker::PhantomData<Z>,
}
#[automock]
#[trait_impl(Foo<T>)]
impl<T: 'static, Z: 'static> Bar<T, Z> {}
#[automock]
impl<T: 'static, Z: 'static> Foo<T> for Bar<T, Z> {
    fn foo(&self, _x: T) -> T {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let mut mock = MockBar::<u32, u64>::new();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(5u32, mock.foo(5u32));
}
