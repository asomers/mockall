// vim: tw=80
//! generic traits
#![deny(warnings)]

use mockall::*;

#[automock]
trait A<T> {
    fn foo(&self, t: T);
    fn bar(&self) -> T;
}

#[test]
fn generic_arguments() {
    let mut mock = MockA::<u32>::new();
    mock.expect_foo()
        .with(mockall::predicate::eq(16u32))
        .once()
        .returning(|_| ());
    mock.foo(16u32);
}

#[test]
fn generic_return() {
    let mut mock = MockA::<u32>::new();
    mock.expect_bar()
        .returning(|| 42u32);
    assert_eq!(42u32, mock.bar());
}

