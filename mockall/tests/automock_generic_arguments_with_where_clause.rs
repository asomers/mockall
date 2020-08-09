// vim: tw=80
//! A method with generic arguments bounded by a where clause
#![deny(warnings)]

use mockall::*;

#[automock]
trait A {
    fn foo<T>(&self, t: T) where T: Clone + 'static;
}

#[test]
fn returning() {
    let mut mock = MockA::new();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| ());
    mock.expect_foo::<i16>()
        .returning(|_x: i16| ());
    mock.foo(5u32);
    mock.foo(-1i16);
}
