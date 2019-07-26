// vim: tw=80

use mockall::*;

mock! {
    Foo<T: 'static> {}
    trait Iterator {
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
