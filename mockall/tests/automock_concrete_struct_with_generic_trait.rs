// vim: tw=80
//! A concrete struct that implements a generic trait
#![deny(warnings)]

use mockall::*;

trait Foo<T: 'static> {
    fn foo(&self, x: T) -> T;
}

struct Bar {}
#[automock]
#[trait_impl(Foo<i32>)]
impl Bar {}
#[automock]
impl Foo<i32> for Bar {
    fn foo(&self, _x: i32) -> i32 {
        unimplemented!()
    }
}

#[test]
fn returning() {
    let mut mock = MockBar::new();
    mock.expect_foo()
        .with(predicate::eq(42))
        .returning(|x| x + 1);
    assert_eq!(43, mock.foo(42));
}
