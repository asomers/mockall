// vim: tw=80
//! Attributes are applied to the mock object, too.

use mockall::*;

pub struct A{}
#[automock]
impl A {
    // Neither A::foo nor MockA::foo should be defined
    #[cfg(target_os = "multics")] pub fn foo(&self, x: DoesNotExist) {}
    // Both A::bar and MockA::bar should be defined
    #[cfg(not(target_os = "multics"))] pub fn bar(&self, _x: i32) -> i32 {0}
}

#[test]
fn returning() {
    let mut mock = MockA::new();
    mock.expect_bar()
        .returning(|x| x);
    assert_eq!(4, mock.bar(4));
}
