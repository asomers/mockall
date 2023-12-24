// vim: tw=80
//! A mocked struct should implement Debug
#![deny(warnings)]

use mockall::*;

#[automock]
pub trait Foo {}

pub trait Bar {}
pub struct Baz {}
#[automock]
#[trait_impl(Bar)]
impl Baz {}
#[automock]
impl Bar for Baz{}

pub struct Bean {}
#[automock]
impl Bean{}

#[test]
fn automock_trait() {
    let foo = MockFoo::new();
    assert_eq!("MockFoo", format!("{foo:?}"));
}

#[test]
fn automock_struct_impl() {
    let bean = MockBean::new();
    assert_eq!("MockBean", format!("{bean:?}"));
}

#[test]
fn automock_trait_impl() {
    let baz = MockBaz::new();
    assert_eq!("MockBaz", format!("{baz:?}"));
}
