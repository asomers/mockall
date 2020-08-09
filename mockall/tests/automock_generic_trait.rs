// vim: tw=80
//! generic traits
#![deny(warnings)]

use mockall::*;

#[automock]
trait A<T: 'static> {
    fn foo(&self);
}

#[test]
fn returning() {
    let mut mock = MockA::<u32>::new();
    mock.expect_foo()
        .returning(|| ());
    mock.foo();
}
