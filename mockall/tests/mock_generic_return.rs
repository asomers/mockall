// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn foo<O: 'static>(&self) -> O;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<i32>().return_const(42i32);
    mock.expect_foo::<u16>().return_const(69u16);
    assert_eq!(42i32, mock.foo());
    assert_eq!(69u16, mock.foo());
}
