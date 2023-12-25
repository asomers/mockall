// vim: tw=80
//! A struct that implements multiple traits
#![deny(warnings)]

use mockall::*;

trait A {}
trait B {}

struct MultiTrait {}
#[automock]
#[trait_impl(A)]
#[trait_impl(B)]
impl MultiTrait {}
#[automock]
impl A for MultiTrait {}
#[automock]
impl B for MultiTrait {}

#[test]
fn new() {
    fn foo<T: A + B>(_t: T) {}

    let mock = MockMultiTrait::new();
    foo(mock);
}
