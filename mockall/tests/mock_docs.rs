// vim: tw=80
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
    trait Tr {
        /// Trait method docs
        fn bar(&self);
    }
}


