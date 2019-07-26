// vim: tw=80
//! A struct with a generic method that has generic arguments

use mockall::*;

trait Foo {
    fn foo<T>(&self, t: T);
}

mock! {
    Foo {
        fn foo<T: 'static>(&self, t: T);
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<i16>().return_const(());
    mock.foo(0i16)
}
