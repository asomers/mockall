// vim: tw=80

use mockall::*;

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


