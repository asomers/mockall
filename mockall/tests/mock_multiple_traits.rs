// vim: tw=80
//! A struct that implements multiple traits
#![deny(warnings)]

use mockall::*;

trait A {}
trait B {}
mock!{
    MultiTrait {}
    impl A for MultiTrait {}
    impl B for MultiTrait {}
}

#[test]
fn new() {
    fn foo<T: A + B>(_t: T) {}

    let mock = MockMultiTrait::new();
    foo(mock);
}
