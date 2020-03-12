// vim: tw=80
//#![deny(missing_docs)]

//! Examples of mock objects and their generated methods.
//!
//! This crate only exists to document the autogenerated methods of the
//! [`Mockall`](https://docs.rs/mockall/latest/mockall)
//! crate.  You should never depend on this crate.
//

#[cfg(doc)]
use mockall::*;

/// A basic trait with several kinds of method.
///
/// It is mocked by the [`MockFoo`](struct.MockFoo.html) struct.
#[cfg(doc)]
#[automock]
pub trait Foo {
    /// A method with a `'static` return type
    fn foo(&self, x: i32, y: i16) -> i32;

    /// A method returning a reference
    fn bar(&self, x: i32) -> &i32;

    /// A method returning a mutable reference
    fn baz(&mut self, x: i32) -> &mut i32;

    /// A method returning a `'static` reference
    fn bean(&self) -> &'static i32;

    /// A static method
    fn bang(x: i32) -> i32;
}

#[cfg(doc)]
#[automock(mod mock_ffi;)]
extern "C" {
    /// A foreign "C" function
    pub fn ffi_func();
}
