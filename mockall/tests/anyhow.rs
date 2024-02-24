// vim: tw=80
//! Mockall should be compatible with crates like Anyhow that redefine `Ok`.
#![deny(warnings)]

use mockall::*;

// Define Error, Result, and Ok similarly to how anyhow defines them
pub struct Error();
impl Error {
    pub fn new<E: std::error::Error>(_e: E) -> Self {
        Self()
    }
}
#[allow(non_snake_case)]
pub fn Ok<T>(t: T) -> Result<T> {
    Result::Ok(t)
}
pub type Result<T, E = Error> = std::result::Result<T, E>;

#[automock]
pub trait Foo {
    fn foo(&self) -> Result<(), Error>;
    fn reffoo(&self) -> &Result<(), Error>;
    fn refmutfoo(&mut self) -> &mut Result<(), Error>;
    fn staticfoo() -> Result<(), Error>;
}

mod static_method {
    use super::*;

    #[test]
    fn ok() {
        let mut foo = MockFoo::new();
        foo.expect_foo()
            .returning(|| Ok(()));
        assert!(foo.foo().is_ok());
    }

    #[test]
    fn err() {
        let mut foo = MockFoo::new();
        foo.expect_foo()
            .returning(|| Err(Error::new(std::io::Error::last_os_error())));
        assert!(foo.foo().is_err());
    }
}

mod ref_method {
    use super::*;

    #[test]
    fn ok() {
        let mut foo = MockFoo::new();
        foo.expect_reffoo()
            .return_const(Ok(()));
        assert!(foo.reffoo().is_ok());
    }

    #[test]
    fn err() {
        let mut foo = MockFoo::new();
        foo.expect_reffoo()
            .return_const(Err(Error::new(std::io::Error::last_os_error())));
        assert!(foo.reffoo().is_err());
    }
}

mod refmut_method {
    use super::*;

    #[test]
    fn ok() {
        let mut foo = MockFoo::new();
        foo.expect_refmutfoo()
            .return_var(Ok(()));
        assert!(foo.refmutfoo().is_ok());
    }

    #[test]
    fn err() {
        let mut foo = MockFoo::new();
        foo.expect_refmutfoo()
            .return_var(Err(Error::new(std::io::Error::last_os_error())));
        assert!(foo.refmutfoo().is_err());
    }
}
