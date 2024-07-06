// vim: tw=80
//! A method that uses `#[auto_enum]`, from the auto_enums crate.
#![deny(warnings)]

use auto_enums::auto_enum;
use futures::{Future, FutureExt, future};
use mockall::*;

pub struct Foo{}

#[automock]
impl Foo {
    #[auto_enum(Future)]
    pub fn sign(&self, x: i32) -> impl Future<Output=i32>
    {
        if x > 0 {
            future::ready(1)
        } else {
            future::ready(1).then(|_| future::ready(-1))
        }
    }
}

#[test]
fn rpit() {
    let mut mock = MockFoo::new();
    mock.expect_sign()
        .returning(|_| {
            Box::pin(future::ready(42))
        });
    let r = mock.sign(101)
        .now_or_never()
        .unwrap();
    assert_eq!(42, r);
}
