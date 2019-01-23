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


