// vim: tw=80
//! Examples of Mockall's generated code
use mockall::{mock, automock};

/// Mock of a basic trait with several kinds of method.
///
/// It is mocked by the [`MockFoo`](struct.MockFoo.html) struct.
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

/// A trait implemented by a Struct we want to mock
pub trait Bah {
    /// Some trait method
    fn bah(&self);
}

mock! {
    /// Mock of a struct
    ///
    /// Structs can be mocked with [`mock!`].
    /// Their mock methods have an identical API to the methods generated by
    /// [`#[automock]`](automock).
    pub Boo {
        /// A method on a struct
        fn boo(&self);
    }
    /// An implementation of a trait on a mocked struct
    impl Bah for Boo {
        fn bah(&self);
    }
}

/// A module full of foreign C functions.
#[automock]
pub mod ffi {
    extern "C" {
        /// A foreign "C" function.
        pub fn ffi_func();
    }
}

/// Mock this entire module
#[automock]
pub mod my_module {
    /// A function in a mocked module
    pub fn modfunc() {
        unimplemented!()
    }
}
