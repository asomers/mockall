// vim: tw=80
//! a method with slice arguments
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo {
    fn foo(&self, x: &[u8]);
}

mod withf {
    use super::*;

    #[test]
    #[cfg_attr(feature = "nightly", should_panic(
            expected = "MockFoo::foo([1, 2, 3, 4]): No matching expectation found"
    ))]
    #[cfg_attr(not(feature = "nightly"), should_panic(
            expected = "MockFoo::foo(?): No matching expectation found"
    ))]
    fn fail() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .withf(|sl| sl == [1, 2, 3])
            .returning(|_| ());
        let x = vec![1, 2, 3, 4];
        mock.foo(&x);
    }

    #[test]
    fn ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .withf(|sl| sl == [1, 2, 3])
            .returning(|_| ());
        let x = vec![1, 2, 3];
        mock.foo(&x);
    }
}
