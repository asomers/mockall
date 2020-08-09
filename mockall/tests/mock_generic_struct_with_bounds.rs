// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock! {
    pub Foo<T: Clone + 'static> {
        fn foo(&self, x: u32) -> i64;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::<i16>::new();
    mock.expect_foo().returning(i64::from);
    assert_eq!(5, mock.foo(5));
}
