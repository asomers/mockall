// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock! {
    Foo<T: 'static> {}
    impl<T: 'static> Iterator for Foo<T> {
        type Item=T;
        fn next(&mut self) -> Option<T>;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::<u32>::new();
    mock.expect_next()
        .return_const(None);
    assert!(mock.next().is_none());
}
