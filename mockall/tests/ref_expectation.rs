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
