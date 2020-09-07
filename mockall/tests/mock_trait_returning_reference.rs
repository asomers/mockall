// vim: tw=80
//! A trait with a method that returns an immutable reference
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self) -> &u32;
}

mock! {
    pub Bar {}
    impl Foo for Bar {
        fn foo(&self) -> &u32;
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::new();
    mock.expect_foo()
        .return_const(5u32);
    assert_eq!(5, *mock.foo());
}
