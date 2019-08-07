// vim: ts=80
//! A generic struct with a generic method on a different parameter

use mockall::*;

mock! {
    Foo<T: 'static> {
        fn foo<Q: 'static>(t: T, q: Q);
    }
}

#[test]
fn with() {
    MockFoo::<u32>::expect_foo::<i16>()
        .with(predicate::eq(42u32), predicate::eq(99i16))
        .returning(|_, _| ());
    MockFoo::foo(42u32, 99i16);
}
