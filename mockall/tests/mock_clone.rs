// vim: tw=80
//! Clone-like methods (non-static method with Self return type) need the return
//! type to be deselfified.
#![deny(warnings)]

use mockall::*;

mock! {
    pub A {}
    impl Clone for A {
        fn clone(&self) -> Self;
    }
}

#[allow(clippy::redundant_clone)]
#[test]
fn returning() {
    let mut mock0 = MockA::new();
    mock0.expect_clone()
        .returning(MockA::new);
    let _mock1 = mock0.clone();
}
