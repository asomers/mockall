// vim: tw=80

use mockall::*;

#[test]
#[should_panic(expected = "No matching expectation found")]
fn missing_expectation() {
    let e = GenericExpectations::default();
    e.called::<i32, u32>(&"foo", 5);
}

/// A MockObject with a method that takes &mut self like:
/// fn foo(&mut self, x: i32) -> u32
#[test]
fn mutable_self() {
    let mut e = GenericExpectations::default();
    let mut count = 0;
    e.expect::<i32, i32>(&"foo")
        .returning(move |x| {
            count += x;
            count
        });
    assert_eq!(5, e.called::<i32, i32>(&"foo", 5));
    assert_eq!(10, e.called::<i32, i32>(&"foo", 5));
}

/// A MockObject with a method that has no arguments or returns
/// fn foo(&self)
#[test]
fn no_args_or_returns() {
    let mut e = GenericExpectations::default();
    e.expect::<(), ()>(&"foo")
        .returning(|_| ());
    e.called::<(), ()>(&"foo", ());
}

/// Expectations return O::default() unless otherwise specified
#[test]
#[cfg_attr(not(feature = "nightly"), ignore)]
fn return_default() {
    let mut e = GenericExpectations::default();
    e.expect::<(), u32>(&"foo");
    let r = e.called::<(), u32>(&"foo", ());
    assert_eq!(u32::default(), r);
}

/// Can't return default for types that aren't Default
#[test]
#[should_panic]
fn return_default_panics_for_non_default_types() {
    struct NonDefault{}
    let mut e = GenericExpectations::default();
    e.expect::<(), NonDefault>(&"foo");
    e.called::<(), NonDefault>(&"foo", ());
}

#[test]
fn return_owned() {
    struct NonCopy{}
    let mut e = GenericExpectations::default();
    let r = NonCopy{};
    e.expect::<(), NonCopy>(&"foo")
        .return_once(|_| r);
    e.called::<(), NonCopy>(&"foo", ());
}

#[test]
#[should_panic(expected = "expected only once")]
fn return_owned_too_many_times() {
    struct NonCopy{}
    let mut e = GenericExpectations::default();
    let r = NonCopy{};
    e.expect::<(), NonCopy>(&"foo")
        .return_once(|_| r);
    e.called::<(), NonCopy>(&"foo", ());
    e.called::<(), NonCopy>(&"foo", ());
}

/// A MockObject with a simple method like:
/// fn foo(&self, x: i32) -> u32
#[test]
fn simple_method() {
    let mut e = GenericExpectations::default();
    e.expect::<i32, u32>(&"foo")
        .returning(|_| 42);
    let r = e.called::<i32, u32>(&"foo", 5);
    assert_eq!(42, r);
}
