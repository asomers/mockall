// vim: tw=80
//! Using QSelf in an argument, a where clause, or a return type
#![deny(warnings)]

use mockall::*;

pub trait Foo {
    type Output;
}

pub struct SendFoo {}
impl Foo for SendFoo {
    type Output = u32;
}

pub struct A{}
#[automock]
impl A {
    pub fn foo<T: Foo + 'static>(&self, _q: <T as Foo>::Output) {
    }
    pub fn bar<T: Foo + 'static>(&self, _t: T) -> <T as Foo>::Output {
        unimplemented!()
    }
    pub fn bean<T>(&self, _t: T)
        where T: Foo + 'static,
              <T as Foo>::Output: Send
    {
    }
}

#[test]
fn arguments() {
    let mut mock = MockA::new();
    mock.expect_foo::<SendFoo>()
        .with(predicate::eq(42u32))
        .return_const(());
    mock.foo::<SendFoo>(42u32);
}

#[test]
fn where_clause() {
    let mut mock = MockA::new();
    mock.expect_bean::<SendFoo>()
        .return_const(());
    mock.bean(SendFoo{});
}

#[test]
fn return_value() {
    let mut mock = MockA::new();
    mock.expect_bar::<SendFoo>()
        .return_const(42u32);
    assert_eq!(42, mock.bar(SendFoo{}));
}
