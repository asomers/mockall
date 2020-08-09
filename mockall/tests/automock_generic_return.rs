// vim: tw=80
//! generic methods with generic return values
#![deny(warnings)]

use mockall::*;

#[automock]
trait A {
    fn foo<T: 'static>(&self, t: T) -> T;
}

#[test]
fn returning() {
    let mut mock = MockA::new();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| 42u32);
    mock.expect_foo::<i16>()
        .returning(|_x: i16| 42i16);
    assert_eq!(42u32, mock.foo(5u32));
    assert_eq!(42i16, mock.foo(-1i16));
}
