// vim: tw=80
#![deny(warnings)]

use mockall::*;


mock! {
    Foo {
        fn foo<T: 'static, Q: 'static>(&self, t: T, q: Q);
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<i16, i32>().return_const(());
    mock.foo(0i16, 1i32)
}
