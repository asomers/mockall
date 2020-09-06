// vim: tw=80
//! A trait with an associated constant
//!
//! https://github.com/asomers/mockall/issues/97
#![deny(warnings)]

use mockall::*;

trait Foo {
    const X: i32;

    fn x_plus_one(&self) -> i32 {
        Self::X + 1
    }
}

mock! {
    Foo {
        const Y: i32 = 69;
        fn foo(&self);
    }
    impl Foo for Foo {
        const X: i32 = 42;
    }
}

#[test]
fn default_method() {
    assert_eq!(MockFoo::new().x_plus_one(), 43);
}

#[test]
fn on_the_struct() {
    assert_eq!(MockFoo::Y, 69);
}
