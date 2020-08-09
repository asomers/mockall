// vim: tw=80
//! A generic struct that implements a generic trait
#![deny(warnings)]

use mockall::*;

trait Foo<T: 'static> {
    fn foo(&self, t: T) -> T;
}

#[allow(unused)]
struct SomeStruct<T: 'static> {
    _t: std::marker::PhantomData<T>
}

#[automock]
impl<T: 'static> Foo<T> for SomeStruct<T> {
    fn foo(&self, t: T) -> T {
        t
    }
}

#[test]
fn returning() {
    let mut mock = MockSomeStruct::<u32>::new();
    mock.expect_foo()
        .returning(|t| t);
    assert_eq!(4, <MockSomeStruct<u32> as Foo<u32>>::foo(&mock, 4));
}
