// vim: tw=80
//! A trait with a constructor method that returns impl Trait
#![deny(warnings)]

use mockall::*;

trait Foo {}

#[allow(unused)]
struct A{}

#[allow(unused)]
struct Bar {}
impl Foo for Bar {}

#[allow(unused)]
#[automock]
impl A {
    fn build() -> impl Foo {
        Bar{}
    }
}

#[test]
fn returning() {
    let ctx = MockA::build_context();
    ctx.expect().returning(|| {
        struct Baz {}
        impl Foo for Baz {}
        Box::new(Baz{})
    });
    let _a = MockA::build();
}
