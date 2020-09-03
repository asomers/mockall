// vim: tw=80
//! A method with `Vec<T>` arguments
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo<T: 'static>(&self, x: Vec<T>);
}

mock! {
    Foo {
        fn foo<T: 'static>(&self, x: Vec<T>);
    }
}

mod withf {
    use super::*;

    #[test]
    #[should_panic(expected = "MockFoo::foo: No matching expectation found")]
    fn fail() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i32>()
            .withf(|sl| sl == &vec![1, 2, 3])
            .return_const(());
        let x = vec![1, 2, 3, 4];
        mock.foo(x);
    }

    #[test]
    fn ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i32>()
            .withf(|sl| sl == &vec![1, 2, 3])
            .return_const(());
        let x = vec![1, 2, 3];
        mock.foo(x);
    }
}
