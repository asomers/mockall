// vim: tw=80

use mockall::*;

#[test]
#[should_panic(expected = "No matching expectation found")]
fn missing_expectation() {
    let e = GenericExpectations::default();
    e.call::<i32, u32>(&"foo", 5);
}

#[test]
fn no_args_or_returns() {
    let mut e = GenericExpectations::default();
    e.expect::<(), ()>(&"foo")
        .returning(|_| ());
    e.call::<(), ()>(&"foo", ());
}

#[test]
fn two_methods() {
    let mut e = GenericExpectations::default();
    e.expect::<(), u32>(&"foo")
        .returning(|_| 5);
    e.expect::<(), u32>(&"bar")
        .returning(|_| 6);
    assert_eq!(6, e.call::<(), u32>(&"bar", ()));
    assert_eq!(5, e.call::<(), u32>(&"foo", ()));
}
