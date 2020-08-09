// vim: tw=80
//! A struct with a constructor method named "new" that has arguments.
//! mockall should mock the provided method, and not autogenerate a 0-argument
//! "new" method.
#![deny(warnings)]

use mockall::*;

pub struct Foo{}

#[automock]
impl Foo {
    #[allow(unused)]
    fn new(_x: u32) -> Self {unimplemented!()}
}

#[test]
fn return_once() {
    let mock = MockFoo::default();
    let ctx = MockFoo::new_context();

    ctx.expect()
        .return_once(|_| mock);

    let _mock = MockFoo::new(5);
}
