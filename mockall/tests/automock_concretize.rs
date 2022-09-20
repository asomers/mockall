// vim: tw=80
//! #[concretize] works with #[automock], too.
#![deny(warnings)]

use mockall::*;
use std::path::{Path, PathBuf};

#[automock]
trait Foo {
    #[concretize]
    fn foo<P: AsRef<std::path::Path>>(&self, x: P);
}

#[automock]
pub mod mymod {
    #[mockall::concretize]
    pub fn bang<P: AsRef<std::path::Path>>(_x: P) { unimplemented!() }
}

mod generic_arg {
    use super::*;

    #[test]
    fn withf() {
        let mut foo = MockFoo::new();
        foo.expect_foo()
            .withf(|p| p.as_ref() == Path::new("/tmp"))
            .times(3)
            .return_const(());
        foo.foo(Path::new("/tmp"));
        foo.foo(PathBuf::from(Path::new("/tmp")));
        foo.foo("/tmp");
    }
}

mod module {
    use super::*;

    #[test]
    fn withf() {
        let ctx = mock_mymod::bang_context();
        ctx.expect()
            .withf(|p| p.as_ref() == Path::new("/tmp"))
            .times(3)
            .return_const(());
        mock_mymod::bang(Path::new("/tmp"));
        mock_mymod::bang(PathBuf::from(Path::new("/tmp")));
        mock_mymod::bang("/tmp");
    }
}
