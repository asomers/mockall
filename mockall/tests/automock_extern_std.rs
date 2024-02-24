// vim: tw=80
//! automocking a trait when std is only an extern crate (eg., as a testing
//! support mod for a no_std library). This setup requires some extra "use"s
//! to make, eg., Box, available.

#![no_std]
extern crate std;

use mockall::*;

#[automock]
pub trait SimpleTrait {
    fn foo(&self, x: u32) -> u32;
}

#[test]
fn creating() {
    let _ = MockSimpleTrait::new();
}
