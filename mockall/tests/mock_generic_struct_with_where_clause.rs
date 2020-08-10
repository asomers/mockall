// vim: tw=80
//! A generic struct with a where clause
#![deny(warnings)]

// An explicit clone is required so as not to return by move
#![allow(clippy::clone_on_copy)]

use mockall::*;

mock! {
    Foo<T: 'static> where T:Clone {
        fn foo(&self, t: T) -> T;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|t: u32| t.clone());
    assert_eq!(5u32, mock.foo(5u32));
}
