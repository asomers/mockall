// vim: tw=80
//! static non-generic methods of generic structs shouldn't require any special
//! treatment when mocking.  Prior to version 0.3.0, the struct's generic
//! parameters had to be duplicated as generic parameters of the method.
#![deny(warnings)]

use mockall::*;

#[automock]
trait Foo<T: 'static> {
    fn foo(t: T);
}

#[test]
fn returning() {
    let ctx = MockFoo::<u32>::foo_context();
    ctx.expect()
        .returning(|_| ());
    MockFoo::foo(42u32);
}
