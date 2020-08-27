// vim: tw=80
//! A non-generic struct can have a generic constructor method
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo {
    fn build<T: 'static>(t: T) -> Self;
}

#[test]
fn returning_once() {
    let ctx = MockFoo::build_context();
    ctx.expect::<i16>()
        .return_once(|_| MockFoo::default());

    let _mock: MockFoo = MockFoo::build::<i16>(-1);
}
