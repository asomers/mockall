// vim: tw=80
//! All types of predicate should work for methods with unsized arguments
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn foo(&self, arg: &str);
    }
}

#[test]
fn with_always() {
    let mut foo = MockFoo::new();
    foo.expect_foo()
        .with(predicate::always())
        .return_const(());

    foo.foo("xxx");
}

#[test]
fn with_eq() {
    let mut foo = MockFoo::new();
    foo.expect_foo()
        .with(predicate::eq("xxx"))
        .return_const(());

    foo.foo("xxx");
}

#[test]
#[should_panic(expected = "MockFoo::foo: No matching expectation found")]
fn with_never() {
    let mut foo = MockFoo::new();
    foo.expect_foo()
        .with(predicate::never())
        .return_const(());

    foo.foo("xxx");
}

#[test]
fn withf() {
    let mut foo = MockFoo::new();
    foo.expect_foo()
        .withf(|a| a == "xxx")
        .return_const(());

    foo.foo("xxx");
}
