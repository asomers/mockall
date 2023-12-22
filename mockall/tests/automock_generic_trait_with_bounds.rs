// vim: tw=80
//! generic traits with bounds on the generic parameters
#![deny(warnings)]

use mockall::*;

#[automock]
trait A<T: Copy> {
    fn foo(&self);
}

#[test]
fn returning() {
    let mut mock = MockA::<u32>::new();
    mock.expect_foo()
        .returning(|| ());
    mock.foo();
}
