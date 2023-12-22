// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo<T> {
    fn foo(&self, x: T) -> T;
}
mock! {
    Bar<T, Z> {}
    impl<T, Z> Foo<T> for Bar<T, Z> {
        fn foo(&self, x: T) -> T;
    }
}

#[test]
fn returning() {
    let mut mock = MockBar::<u32, u64>::new();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(5u32, mock.foo(5u32));
}
