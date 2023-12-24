// vim: tw=80
#![deny(warnings)]

use mockall::*;

trait Foo {
    fn foo(&self, _x: u32) -> i64;
}

mock!{
    #[derive(Debug)]
    pub Bar {
        pub fn bar(&self, _x: i64) -> u32;
    }
    impl Foo for Bar {
        fn foo(&self, _x: u32) -> i64;
    }
}

#[test]
fn return_const() {
    let mut mock = MockBar::new();
    mock.expect_foo()
        .return_const(6);
    assert_eq!(6, mock.foo(5));
}

