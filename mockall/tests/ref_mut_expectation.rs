// vim: tw=80

use mockall::*;

#[test]
fn match_eq_ok() {
    let mut e = RefMutExpectation::<(i32), i32>::default();
    e.return_var(99i32);
    e.with(predicates::ord::eq(5));
    e.call_mut(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    let mut e = RefMutExpectation::<(i32), i32>::default();
    e.return_var(99i32);
    e.with(predicates::ord::eq(4));
    e.call_mut(5);
}

#[test]
fn never_ok() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.never();
}

#[test]
#[should_panic(expected = "Expectation should not have been called")]
fn never_fail() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.never();
    e.call_mut(());
}

#[test]
fn return_mutable_reference() {
    let mut e = RefMutExpectation::<(), i32>::default();
    e.returning(|_| 5i32);
    assert_eq!(5i32, *e.call_mut(()));
}

#[test]
fn return_mutable_reference_return_var() {
    let mut e = RefMutExpectation::<(), i32>::default();
    e.return_var(5i32);
    assert_eq!(5i32, *e.call_mut(()));
}

#[test]
fn times_any() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.times(1);
    e.times_any();
    e.call_mut(());
    e.call_mut(());
}

#[test]
fn times_ok() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.times(2);
    e.call_mut(());
    e.call_mut(());
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_too_few() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.times(2);
    e.call_mut(());
}

#[test]
#[should_panic(expected = "Expectation called more than 2 times")]
fn times_too_many() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.times(2);
    e.call_mut(());
    e.call_mut(());
    e.call_mut(());
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}

#[test]
fn times_range_ok() {
    let mut e0 = RefMutExpectation::<(), ()>::default();
    e0.returning(|_| ());
    e0.times_range(2..4);
    e0.call_mut(());
    e0.call_mut(());

    let mut e1 = RefMutExpectation::<(), ()>::default();
    e1.returning(|_| ());
    e1.times_range(2..4);
    e1.call_mut(());
    e1.call_mut(());
    e1.call_mut(());
}

#[test]
#[should_panic(expected = "Expectation called fewer than 2 times")]
fn times_range_too_few() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.times_range(2..4);
    e.call_mut(());
}

#[test]
#[should_panic(expected = "Expectation called more than 3 times")]
fn times_range_too_many() {
    let mut e = RefMutExpectation::<(), ()>::default();
    e.returning(|_| ());
    e.times_range(2..4);
    e.call_mut(());
    e.call_mut(());
    e.call_mut(());
    e.call_mut(());
    // Verify that we panic quickly and don't reach code below this point.
    panic!("Shouldn't get here!");
}
