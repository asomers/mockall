// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self, x: u32) -> i64;
}

mock!{
    pub Bar {}
    impl Foo for Bar {
        fn foo(&self, x: u32) -> i64;
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::new();
    mock.expect_foo()
        .return_const(6);
    assert_eq!(6, mock.foo(5));
}

