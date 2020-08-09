// vim: tw=80
//! automatic-style mocking with associated types, with QSelf
#![deny(warnings)]

use mockall::*;

trait SomeTrait<Q>{}
struct Foo {}
impl SomeTrait<u32> for Foo {}

#[automock(type T=u32;)]
trait A {
    type T: Clone;
    fn baz(&self) -> Box<dyn SomeTrait<<Self as A>::T>>;
}

#[test]
fn returning() {
    let mut mock = MockA::new();
    mock.expect_baz()
        .returning(|| Box::new(Foo{}));
    mock.baz();
}
