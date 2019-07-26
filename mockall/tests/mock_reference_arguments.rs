// vim: tw=80
//! A struct with methods that take arguments by reference

use mockall::*;

const X: u32 = 99;

mock!{
    Foo {
        fn foo(&self, x: &u32) -> u32;
        fn bar(&self, y: &'static u32);
    }
}

#[test]
fn withf() {
    const Y: u32 = 5;
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .withf(|x| *x == 5)
        .returning(|x| *x);
    mock.expect_bar()
        .withf(|x| *x == 99)
        .returning(|_| ());
    let r = mock.foo(&Y);
    assert_eq!(5, r);
    mock.bar(&X);
}
