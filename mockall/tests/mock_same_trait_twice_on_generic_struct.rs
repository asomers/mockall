// vim: ts=80
//! Mock the same generic trait twice on a single struct (with different generic
//! arguments, of course).
//!
#![deny(warnings)]
#![allow(clippy::from_over_into)]

use mockall::*;

mock! {
    pub Foo<T> {}
    impl Into<u32> for Foo<u32> {
        fn into(self) -> u32;
    }
    impl Into<i32> for Foo<i32> {
        fn into(self) -> i32;
    }
}

/// Ensure we can set expectations for both methods simultaneously
#[test]
fn return_once() {
    let mut mocku = MockFoo::<u32>::new();
    mocku.expect_into()
        .return_once(|| 42);
    let mut mocki = MockFoo::<i32>::new();
    mocki.expect_into()
        .return_once(|| -42);

    assert_eq!(<MockFoo<u32> as Into<u32>>::into(mocku), 42u32);
    assert_eq!(<MockFoo<i32> as Into<i32>>::into(mocki), -42);
}
