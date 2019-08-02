// vim: tw=80
//! Clone-like methods (non-static method with Self return type) need the return
//! type to be deselfified.

use mockall::*;

mock! {
    pub A {}
    trait Clone {
        fn clone(&self) -> Self;
    }
}

#[test]
fn returning() {
    let mut mock0 = MockA::new();
    mock0.expect_clone()
        .returning(MockA::new);
    let _mock1 = mock0.clone();
}
