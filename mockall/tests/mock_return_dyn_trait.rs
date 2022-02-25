// vim: tw=80
//! a method that returns a reference to a trait object
#![deny(warnings)]

use mockall::*;
use std::fmt::Debug;

trait T: Debug + Sync {
    fn mutate(&mut self) {}
}

impl T for u32 {}
trait Base {
    fn as_t(&self) -> &dyn T;
}
trait Derived: Base {
    fn as_base(&self) -> &dyn Base;
}

impl Base for u32 {
    fn as_t(&self) -> &dyn T {
        self
    }
}
impl Derived for u32 {
    fn as_base(&self) -> &dyn Base {
        self
    }
}

impl<Q> T for Q where Q: Debug + Sync + AsMut<dyn T> {}

mock!{
    Foo {
        fn foo(&self) -> &dyn Debug;
        fn bar(&self) -> &'static dyn T;
        fn baz(&mut self) -> &mut dyn T;
    }
}

mock! {
    Bar {}
    impl Base for Bar {
        fn as_t(&self) -> &dyn T;
    }
    impl Derived for Bar {
        fn as_base(&self) -> &dyn Base;
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

#[test]
fn dyn_upcast() {
    let mut mock = MockBar::new();
    mock.expect_as_t()
        .return_const(Box::new(42u32) as Box<dyn T>);

    assert_eq!("42", format!("{:?}", mock.as_t()));
}
