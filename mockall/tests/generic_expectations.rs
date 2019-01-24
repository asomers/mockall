// vim: tw=80

use mockall::*;

#[test]
#[should_panic(expected = "No matching expectation found")]
fn missing_expectation() {
    let e = GenericExpectation::default();
    e.call::<i32, u32>(5);
}

#[test]
fn no_args_or_returns() {
    let mut e = GenericExpectation::default();
    e.expect::<(), ()>()
        .returning(|_| ());
    e.call::<(), ()>(());
}
