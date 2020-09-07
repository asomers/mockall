// vim: tw=80
//! A struct with a generic method that has generic arguments
#![deny(warnings)]

use mockall::*;

mock! {
    Foo {
        fn foo<T: 'static>(&self, t: T) -> i32;
        fn bar<T: 'static>(&self, t: T) -> i32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<i16>()
        .return_const(0);
    assert_eq!(0, mock.foo(0i16));
}

mod with {
    use super::*;

    /// Multiple generic methods with the same set of generic parameters are in
    /// scope at the same time.  Their expectations should not get scrambled.
    #[test]
    fn multiple_methods() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i16>()
            .with(predicate::eq(4))
            .return_const(0);
        mock.expect_bar::<i16>()
            .with(predicate::eq(5))
            .return_const(0);
        mock.bar(5i16);
        mock.foo(4i16);
    }

    #[test]
    fn ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i16>()
            .with(predicate::eq(4))
            .return_const(0);
        mock.foo(4i16);
    }

    #[test]
    #[should_panic(expected = "MockFoo::foo: No matching expectation found")]
    fn wrong_generic_type() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i16>()
            .with(predicate::eq(4))
            .return_const(0);
        mock.foo(4i32);
    }
}
