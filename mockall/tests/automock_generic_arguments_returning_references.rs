// vim: tw=80
//! generic methods with generic arguments returning immutable references
#![deny(warnings)]

use mockall::*;

#[automock]
trait A {
    fn foo<T: 'static>(&self, t: T) -> &u32;
}

#[test]
fn return_const() {
    let mut mock = MockA::new();
    mock.expect_foo::<u32>().return_const(5);
    assert_eq!(5, *mock.foo(42u32));
}
