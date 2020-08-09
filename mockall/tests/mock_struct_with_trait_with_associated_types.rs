// vim: tw=80
#![deny(warnings)]

use mockall::*;

mock! {
    pub MyIter {}
    trait Iterator {
        type Item=u32;

        fn next(&mut self) -> Option<u32>;
    }
}

#[test]
fn return_const() {
    let mut mock = MockMyIter::new();
    mock.expect_next()
        .return_const(None);
    assert!(mock.next().is_none());
}
