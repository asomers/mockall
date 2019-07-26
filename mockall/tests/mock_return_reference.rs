// vim: tw=80
//! A struct with a method that returns an immutable reference

use mockall::*;

mock! {
    Foo {
        fn foo(&self) -> &u32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(5u32);
    assert_eq!(5, *mock.foo());
}
