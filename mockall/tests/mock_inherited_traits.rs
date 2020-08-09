// vim: tw=80
//! A struct that implements a trait that inherits another trait
#![deny(warnings)]

use mockall::*;

trait A {
    fn foo(&self);
}

trait B: A {
    fn bar(&self);
}

mock!{
    B {}
    trait A {
        fn foo(&self);
    }
    trait B {
        fn bar(&self);
    }
}

#[test]
fn returning() {
    let mut mock = MockB::new();
    mock.expect_foo().returning(|| ());
    mock.expect_bar().returning(|| ());
    mock.foo();
    mock.bar();
}
