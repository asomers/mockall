// vim: ts=80
#![deny(warnings)]

use mockall::*;

#[allow(clippy::missing_safety_doc)]
pub unsafe trait Bar {
    fn bar(&self) -> i32;
}

mock! {
    pub Foo{}
    unsafe impl Bar for Foo {
        fn bar(&self) -> i32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_bar()
        .return_const(42);

    assert_eq!(42, mock.bar());
}
