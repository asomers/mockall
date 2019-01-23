// vim: tw=80

use mockall::*;

#[test]
fn match_eq_ok() {
    let mut e = RefExpectation::<(i32), i32>::default();
    e.return_const(99i32);
    e.with(predicates::ord::eq(5));
    e.call(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    let mut e = RefExpectation::<(i32), i32>::default();
    e.return_const(99i32);
    e.with(predicates::ord::eq(4));
    e.call(5);
}

#[test]
fn return_reference() {
    let mut e = RefExpectation::<(), i32>::default();
    e.return_const(5i32);
    assert_eq!(5i32, *e.call(()));
}

#[test]
fn times_ok() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.times(2);
    e.call(());
    e.call(());
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.times(2);
    e.call(());
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_too_many() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.times(2);
    e.call(());
    e.call(());
    e.call(());
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
