// vim: tw=80
//! Methods with multiple generic lifetime parameters should produce their
//! generated code deterministically.
//!
//! This test is designed to work with "--cfg reprocheck"

#![deny(warnings)]
#![allow(clippy::needless_lifetimes)]

use mockall::*;

#[automock]
pub trait Foo {
    fn foo<'a, 'b, 'c, 'd, 'e, 'f>(&self, x: &'a &'b &'c &'d &'e &'f i32);
}
