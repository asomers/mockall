// vim: tw=80
//! A struct that implements multiple traits with same name
#![deny(warnings)]

use mockall::*;

mod a {
    pub trait Trait {}
}

mod b {
    pub trait Trait {}
}

mock! {
    MultiTrait {}
    impl a::Trait for MultiTrait {}
    impl b::Trait for MultiTrait {}
}

#[test]
fn new() {
    fn foo<T: a::Trait + b::Trait>(_t: T) {}

    let mock = MockMultiTrait::new();
    foo(mock);
}
