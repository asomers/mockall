// vim: tw=80
//! A struct that implements multiple traits

use mockall::*;

trait A {}
trait B {}
mock!{
    MultiTrait {}
    trait A  {}
    trait B  {}
}

#[test]
fn new() {
    fn foo<T: A + B>(_t: T) {}

    let mock = MockMultiTrait::new();
    foo(mock);
}
