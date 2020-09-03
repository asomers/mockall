// vim: tw=80
//! A method with `String` arguments
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo {
    fn foo(&self, x: String);
}

mod withf {
    use super::*;

    #[test]
    #[should_panic(expected = "MockFoo::foo: No matching expectation found")]
    fn fail() {
        let mut mock = MockFoo::new();
        mock.expect_foo().withf(|sl| sl == "abc").return_const(());
        let x = String::from("abcd");
        mock.foo(x);
    }

    #[test]
    fn ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo().withf(|sl| sl == "abc").return_const(());
        let x = String::from("abc");
        mock.foo(x);
    }
}
