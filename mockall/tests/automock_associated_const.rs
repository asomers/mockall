// vim: tw=80
//! A trait with an associated constant
//!
//! It's not possible to automock the trait, like:
//! ```
//! #[automock]
//!     trait Foo {
//!     const X: i32;
//! }
//! ```
//! because there's no way to set the value of X on MockFoo.
//!
//! But it _is_ possible to automock the trait implementation, like this:
//! ```
//! struct Bar {}
//! #[automock]
//! impl Foo for Bar {
//!     const X: i32;
//! }
//! ```
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

#[allow(dead_code)]
struct Bar {}

#[automock]
impl Foo for Bar {
    const X: i32 = 42;
}

#[allow(dead_code)]
pub struct Baz {}
#[automock]
impl Baz {
    pub const Y: i32 = 69;
}

#[test]
fn default_method() {
    assert_eq!(MockBar::new().x_plus_one(), 43);
}

#[test]
fn on_a_struct() {
    assert_eq!(MockBaz::Y, 69);
}
