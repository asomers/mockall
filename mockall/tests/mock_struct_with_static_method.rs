// vim: tw=80

use mockall::*;

mock!{
    Foo {
        fn bar(x: u32) -> u64;
        fn baz(x: u16) -> u8;
    }
}

#[test]
#[should_panic(expected =
    "MockFoo::bar: Expectation called fewer than 1 times")]
fn checkpoint() {
    let mut mock = MockFoo::new();
    MockFoo::expect_bar()
        .returning(|_| 32)
        .times(1..3);
    mock.checkpoint();
    panic!("Shouldn't get here!");
}

// Until we get some kind of context for free functions, the return_default
// feature won't work if there are any other tests that mock the same function.
// https://github.com/asomers/mockall/issues/30
#[cfg_attr(not(feature = "nightly"),
           should_panic(expected = "Returning default values requires"))]
#[test]
fn return_default() {
    MockFoo::expect_baz();
    let r = MockFoo::baz(5);
    assert_eq!(u8::default(), r);
}

#[test]
fn returning() {
    MockFoo::expect_bar()
        .returning(|x| u64::from(x + 1));
    assert_eq!(42, MockFoo::bar(41));
}
