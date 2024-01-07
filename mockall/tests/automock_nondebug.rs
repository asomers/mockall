// vim: tw=80
//! A method may have non-Debug arguments and/or return values.
#![deny(warnings)]

use mockall::*;

// Don't derive Debug
pub struct NonDebug{}

#[automock]
pub trait Foo {
    fn foo(&self, x: NonDebug);
}

#[test]
#[should_panic(expected = "MockFoo::foo(?): No matching expectation found")]
fn with_no_matches() {
    let mock = MockFoo::new();
    mock.foo(NonDebug{});
}


