// vim: tw=80
//! A method that returns a type which is a common target for std::ops::Deref
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn foo(&self) -> &str;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const("Stuff".to_owned());
    assert_eq!("Stuff", mock.foo());
}
