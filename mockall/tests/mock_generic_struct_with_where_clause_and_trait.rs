// vim: tw=80
//! A generic struct with a where clause, that also implements a trait
#![deny(warnings)]

// An explicit clone is required so as not to return by move
#![allow(clippy::clone_on_copy)]

use mockall::*;

trait Bar {
    fn bar(&self);
}
mock! {
    Foo<T: 'static> where T: Clone {
        fn foo(&self, t: T) -> T;
    }
    impl<T: 'static> Bar for Foo<T> where T: Clone {
        fn bar(&self);
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|t: u32| t.clone());
    mock.expect_bar()
        .returning(|| ());
    assert_eq!(5u32, mock.foo(5u32));
    mock.bar();
}
