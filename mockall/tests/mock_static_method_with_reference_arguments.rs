// vim: tw=80

use mockall::*;

mock!{
    Foo {
        fn bar(x: &u32) -> u64;
    }
}

#[test]
fn with() {
    MockFoo::expect_bar()
        .with(predicate::eq(42))
        .return_const(99u64);
    assert_eq!(99, MockFoo::bar(&42));
}
