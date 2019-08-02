// vim: ts=80

use mockall::*;

trait Foo {
    fn foo(&self, x: u32) -> u32;
}

mock! {
    Bar<T: Copy + 'static> {}
    trait Foo {
        fn foo(&self, x: u32) -> u32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::<u32>::new();
    mock.expect_foo()
        .return_const(43u32);
    assert_eq!(43, mock.foo(42));
}
