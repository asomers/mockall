// vim: tw=80
#![deny(missing_docs)]
#![deny(warnings)]

use mockall::*;

// mock! should allow doc comments in all reasonable positions.  This test
// ensures that the code will compile.  mockall_derive has a unit test to ensure
// that the doc comments are correctly placed.

trait Tr {
    fn bar(&self);
}

mock!{
    /// Struct docs
    pub Foo {
        /// Method docs
        fn foo(&self);
    }
    /// Trait docs
    impl Tr for Foo {
        /// Trait method docs
        fn bar(&self);
    }
}
