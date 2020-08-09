// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock!{
    pub Foo<T: Clone + 'static> {
        fn foo<Q: 'static>(&self, q: Q) -> T;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::<u32>::new();
    mock.expect_foo::<i16>()
        .return_const(100_000u32);
    assert_eq!(100_000, mock.foo(-5i16));
}
