// vim: tw=80
//! mock a method whose self parameter has an explicit lifetime
//! https://github.com/asomers/mockall/issues/95
#![deny(warnings)]
#![allow(single_use_lifetimes)] // The lifetime is the whole point of this test

use mockall::*;

mock!{
    Foo {
        fn foo<'life0>(&'life0 self, x: u32) -> u32;
    }
}

// TODO: test that the mock method respects the lifetime bound, once Mockall
// supports mocking non-static structs
// (https://github.com/asomers/mockall/issues/4)
