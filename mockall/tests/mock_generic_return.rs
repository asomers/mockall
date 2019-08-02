// vim: tw=80

use mockall::*;

trait Foo {
    fn foo<O>(&self) -> O;
}

mock! {
    Foo {
        fn foo<O: 'static>(&self) -> O;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<i32>().return_const(42);
    assert_eq!(42i32, mock.foo());
}
