// vim: tw=80
//! A method may have non-Debug arguments and/or return values.
#![deny(warnings)]

use std::fmt::Display;

use mockall::*;

#[derive(Debug)]
pub struct DisplayOverDebug(u32);

impl Display for DisplayOverDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "five")
    }
}

#[automock]
pub trait Foo {
    fn foo(&self, x: DisplayOverDebug);
}

#[test]
#[should_panic(expected = "MockFoo::foo(five): No matching expectation found")]
fn with_no_matches() {
    let mock = MockFoo::new();
    mock.foo(DisplayOverDebug(5));
}
