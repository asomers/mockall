// vim: tw=80
//! A struct with a constructor method named "new" that has arguments.
//! mockall should mock the provided method, and not autogenerate a 0-argument
//! "new" method.
#![deny(warnings)]

use mockall::*;

mock! {
    pub Foo {
        fn foo(&self) -> u32;
        fn new(x: u32) -> Self;
    }
}

#[test]
fn returning_once() {
    let mock = MockFoo::default();

    let ctx = MockFoo::new_context();
    ctx.expect()
        .return_once(|_| mock);

    let _mock = MockFoo::new(5);
}
