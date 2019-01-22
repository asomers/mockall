// vim: tw=80

use mockall::*;

#[test]
fn return_reference() {
    let mut e = RefExpectation::<(), i32>::default();
    e.return_const(5i32);
    assert_eq!(5i32, *e.call(()));
}
