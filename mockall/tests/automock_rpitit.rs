// vim: tw=80
//! Return position Impl Trait in Traits
//! https://rust-lang.github.io/rfcs/3425-return-position-impl-trait-in-traits.html
#![cfg(feature = "nightly")]
#![deny(warnings)]

use std::fmt::Debug;

use mockall::*;

#[automock]
trait Foo {
    fn bar(&self) -> impl Debug;
}

#[test]
fn returning() {
    let mut foo = MockFoo::default();
    foo.expect_bar()
        .returning(|| Box::new(42u32));
    assert_eq!("42", format!("{:?}", foo.bar()));
}
