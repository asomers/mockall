// vim: tw=80
//! A trait that uses tracing::instrument should be automockable.  The mock
//! method won't be instrumented, though.
#![deny(warnings)]

use mockall::*;
use tracing::instrument;

#[derive(Debug)]
pub struct Foo {}

#[automock]
impl Foo {
    #[instrument]
    fn foo(&self) {}
    #[instrument]
    fn bar() {}
    #[tracing::instrument]
    fn fooz(&self) {}
    #[tracing::instrument]
    fn barz() {}
}
