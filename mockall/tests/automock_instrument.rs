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
    pub fn foo(&self) {}
    #[instrument]
    pub fn bar() {}
    #[tracing::instrument]
    pub fn fooz(&self) {}
    #[tracing::instrument]
    pub fn barz() {}
}
