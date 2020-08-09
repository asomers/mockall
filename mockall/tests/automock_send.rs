// vim: tw=80
//! A mock object should be Send
#![deny(warnings)]

use mockall::*;

#[automock]
trait T {
    fn foo(&self) -> u32;
}

#[test]
#[allow(clippy::unnecessary_operation)] // The cast is the whole point
fn cast_to_send() {
    let mock = MockT::new();
    let _m = Box::new(mock) as Box<dyn T + Send>;
}
