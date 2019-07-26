// vim: tw=80
//! Structs can be mocked with mock!  This is useful when the struct's original
//! definition is not accessible.

use mockall::*;

// A struct with a definition like this:
// struct Foo {
//     _x: i16
// }
// impl Foo {
//     fn foo(&self, _x: u32) -> u32 {
//         42
//     }
// }
// Could be mocked like this:
mock!{
    Foo {
        fn foo(&self, x: u32) -> u32;
    }
}

mod checkpoint {
    use super::*;

    #[test]
    fn expect_again() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 5)
            .times_range(1..3);
        mock.foo(0);
        mock.checkpoint();

        mock.expect_foo()
            .returning(|_| 25);
        assert_eq!(25, mock.foo(0));
    }

    #[test]
    #[should_panic(expected = "Expectation called fewer than 1 times")]
    fn not_yet_satisfied() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 42)
            .times(1);
        mock.checkpoint();
        panic!("Shouldn't get here!");
    }


    #[test]
    fn ok() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 5)
            .times_range(1..3);
        mock.foo(0);
        mock.checkpoint();
    }

    #[test]
    #[should_panic(expected = "No matching expectation found")]
    fn removes_old_expectations() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|_| 42)
            .times_range(1..3);
        mock.foo(0);
        mock.checkpoint();
        mock.foo(0);
        panic!("Shouldn't get here!");
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(6, mock.foo(5));
}

