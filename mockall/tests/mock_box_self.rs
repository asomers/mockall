// vim: tw=80
//! Methods that take receivers like Box<Self> instead of &self
#![allow(clippy::borrowed_box, clippy::boxed_local)]
#![deny(warnings)]

use mockall::*;
use std::sync::Arc;
use std::pin::Pin;
use std::rc::Rc;

mock! {
    Foo {
        fn foo(self: &Box<Self>);
        fn baz(mut self: Box<Self>);
        fn bar(self: Box<Self>);
        fn bean(self: Arc<Self>);
        fn booz(self: Pin<Box<Self>>);
        fn blez(self: Rc<Self>);
    }
}

#[test]
fn arc() {
    let mut mock = MockFoo::new();
    mock.expect_bean()
        .returning(|| ());
    Arc::new(mock).bean();
}

#[test]
fn pin() {
    let mut mock = MockFoo::new();
    mock.expect_booz()
        .returning(|| ());
    Pin::new(Box::new(mock)).booz();
}

#[test]
fn rc() {
    let mut mock = MockFoo::new();
    mock.expect_blez()
        .returning(|| ());
    Rc::new(mock).blez();
}

#[test]
fn ref_box() {
    let mut mock = Box::new(MockFoo::new());
    mock.expect_foo()
        .returning(|| ());
    mock.foo();
}

#[test]
fn mutable() {
    let mut mock = Box::new(MockFoo::new());
    mock.expect_baz()
        .returning(|| ());
    mock.baz();
}

#[test]
fn owned() {
    let mut mock = Box::new(MockFoo::new());
    mock.expect_bar()
        .returning(|| ());
    mock.bar();
}
