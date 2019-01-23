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
fn never_ok() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.never();
    e.call(());
}

#[test]
fn return_reference() {
    let mut e = RefExpectation::<(), i32>::default();
    e.return_const(5i32);
    assert_eq!(5i32, *e.call(()));
}

#[test]
fn times_any() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.times(1);
    e.times_any();
    e.call(());
    e.call(());
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
#[should_panic(expected = "Expectation called more than 2 times")]
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

#[test]
fn times_range_ok() {
    let mut e0 = RefExpectation::<(), ()>::default();
    e0.return_const(());
    e0.times_range(2..4);
    e0.call(());
    e0.call(());

    let mut e1 = RefExpectation::<(), ()>::default();
    e1.return_const(());
    e1.times_range(2..4);
    e1.call(());
    e1.call(());
    e1.call(());
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_range_too_few() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.times_range(2..4);
    e.call(());
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    let mut e = RefExpectation::<(), ()>::default();
    e.return_const(());
    e.times_range(2..4);
    e.call(());
    e.call(());
    e.call(());
    e.call(());
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
