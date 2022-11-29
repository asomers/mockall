// vim: ts=80
//! Methods with a "where Self: ..." where clause should be mocked as concrete,
//! not generic.
#![deny(warnings)]
#![allow(clippy::needless_lifetimes)]

// Enclose the mocked trait within a non-public module.  With some versions of
// rustc, that causes "unused method" errors for the generic code, but not the
// concrete code.
// rustc 1.66.0-nightly (e7119a030 2022-09-22)
mod mymod {

    #[mockall::automock]
    pub trait Server {
        fn version<'a>(&'a self) -> Option<&'static str> where Self: Sized;
    }

}

use mymod::{MockServer, Server};

#[test]
fn return_const() {
    let mut mock = MockServer::new();
    mock.expect_version()
        .return_const(None);

    mock.version();
}
