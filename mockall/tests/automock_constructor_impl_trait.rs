// vim: tw=80
//! A trait with a constructor method that returns impl Trait

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
    MockA::expect_build().returning(|| {
        struct Baz {}
        impl Foo for Baz {}
        Box::new(Baz{})
    });
    let _a = MockA::build();
}
