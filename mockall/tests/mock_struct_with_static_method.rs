// vim: tw=80

use mockall::*;

mock!{
    Foo {
        fn bar(x: u32) -> u64;
        fn baz(x: u16) -> u8;
    }
}

#[test]
#[should_panic(expected = "Expectation called fewer than 1 times")]
fn checkpoint() {
    let mut mock = MockFoo::new();
    MockFoo::expect_bar()
        .returning(|_| 32)
        .times(1..3);
    mock.checkpoint();
    panic!("Shouldn't get here!");
}

#[test]
fn returning() {
    MockFoo::expect_bar()
        .returning(|x| u64::from(x + 1));
    assert_eq!(42, MockFoo::bar(41));
}
