// vim: tw=80
//! A trait with generic parameters bounded by a where clause
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo<T> where T: Clone + 'static {
    fn foo(&self);
}

#[test]
fn returning() {
    let mut mock = MockFoo::<u8>::default();
    mock.expect_foo()
        .returning(|| ());
    mock.foo();
}
