// vim: tw=80
//! A struct with a method that returns a mutable reference

use mockall::*;

mock! {
    Foo {
        fn foo(&mut self, i: u32) -> &mut u32;
    }
}

#[test]
fn return_var() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .return_var(5u32);
    {
        let r = mock.foo(0);
        assert_eq!(5, *r);
        *r = 6;
    }
    assert_eq!(6, *mock.foo(0));
}
