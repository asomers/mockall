// vim: tw=80
//! A struct with methods that take arguments by reference
#![deny(warnings)]

use mockall::*;

const X: u32 = 99;

mock!{
    Foo {
        fn foo(&self, x: &u32) -> u32;
        fn bar(&self, y: &'static u32);
    }
}

mod r#match {
    use super::*;

    #[test]
    fn with() {
        const Y: u32 = 5;
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .with(predicate::eq(5u32))
            .returning(|x| *x);
        mock.expect_bar()
            .with(predicate::eq(99u32))
            .returning(|_| ());
        let r = mock.foo(&Y);
        assert_eq!(5, r);
        mock.bar(&X);
    }

    #[test]
    fn withf() {
        const Y: u32 = 5;
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .withf(|x| *x == 5)
            .returning(|x| *x);
        mock.expect_bar()
            .withf(|x| *x == 99)
            .returning(|_| ());
        let r = mock.foo(&Y);
        assert_eq!(5, r);
        mock.bar(&X);
    }
}

mod times {
    use super::*;
    const X: u32 = 42;

    #[test]
    fn ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 0)
            .times(2);
        mock.foo(&X);
        mock.foo(&X);
    }

    #[test]
    #[should_panic(expected =
                   "MockFoo::foo: Expectation(<anything>) called fewer than 2 times")]
    fn too_few() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 0)
            .times(2);
        mock.foo(&X);
    }

    #[test]
    #[should_panic(expected =
        "MockFoo::foo: Expectation(<anything>) called more than 2 times")]
    fn too_many() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 0)
            .times(2);
        mock.foo(&X);
        mock.foo(&X);
        mock.foo(&X);
        // Verify that we panic quickly and don't reach code below this point.
        panic!("Shouldn't get here!");
    }

    #[test]
    fn range_ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 0)
            .times(2..4);
        mock.foo(&X);
        mock.foo(&X);
    }

    #[test]
    #[should_panic(expected =
                   "MockFoo::foo: Expectation(<anything>) called fewer than 2 times")]
    fn range_too_few() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 0)
            .times(2..4);
        mock.foo(&X);
    }

    #[test]
    #[should_panic(expected =
        "MockFoo::foo: Expectation(<anything>) called more than 3 times")]
    fn range_too_many() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 0)
            .times(2..4);
        mock.foo(&X);
        mock.foo(&X);
        mock.foo(&X);
        mock.foo(&X);
        // Verify that we panic quickly and don't reach code below this point.
        panic!("Shouldn't get here!");
    }
}

#[test]
fn times_full() {
    const X: u32 = 42;
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|_| 0)
        .times(1)
        .times(..);
    mock.foo(&X);
    mock.foo(&X);
}
