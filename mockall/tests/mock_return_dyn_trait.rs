// vim: tw=80
//! a method that returns a reference to a trait object
#![deny(warnings)]

use mockall::*;
use std::fmt::Debug;

trait T: Debug + Sync {
    fn mutate(&mut self) {}
}

impl T for u32 {}

impl<Q> T for Q where Q: Debug + Sync + AsMut<dyn T> {}

mock!{
    Foo {
        fn foo(&self) -> &dyn Debug;
        fn bar(&self) -> &'static dyn T;
        fn baz(&mut self) -> &mut dyn T;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_const(Box::new(42u32) as Box<dyn Debug>);

    assert_eq!("42", format!("{:?}", mock.foo()));
}

#[test]
fn static_ref() {
    let mut mock = MockFoo::new();
    mock.expect_bar()
        .return_const(&42u32 as &dyn T);

    assert_eq!("42", format!("{:?}", mock.bar()));
}

#[test]
fn return_var() {
    let mut mock = MockFoo::new();
    mock.expect_baz()
        .return_var(Box::new(42u32) as Box<dyn T>);

    mock.baz().mutate();
}
