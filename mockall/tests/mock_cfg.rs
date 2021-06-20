// vim: tw=80
//! mock's methods and trait impls can be conditionally compiled
#![deny(warnings)]

use mockall::*;

// For this test, use the "nightly" feature as the cfg gate, because it's tested
// both ways in CI.
#[cfg(feature = "nightly")]
trait Beez {
    fn beez(&self);
}
#[cfg(not(feature = "nightly"))]
trait Beez {
    fn beez(&self, x: i32) -> i32;
}

mock! {
    pub Foo {
        #[cfg(feature = "nightly")]
        fn foo(&self);
        #[cfg(not(feature = "nightly"))]
        fn foo(&self, x: i32) -> i32;
    }
    #[cfg(feature = "nightly")]
    impl Beez for Foo {
        fn beez(&self);
    }
    #[cfg(not(feature = "nightly"))]
    impl Beez for Foo {
        fn beez(&self, x: i32) -> i32;
    }
}

#[test]
#[cfg(feature = "nightly")]
fn test_nightly_method() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|| ());
}

#[test]
#[cfg(feature = "nightly")]
fn test_nightly_trait() {
    let mut mock = MockFoo::new();
    mock.expect_beez()
        .returning(|| ());
}

#[test]
#[cfg(not(feature = "nightly"))]
fn test_not_nightly_method() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|x| x + 1);
}

#[test]
#[cfg(not(feature = "nightly"))]
fn test_not_nightly_trait() {
    let mut mock = MockFoo::new();
    mock.expect_beez()
        .returning(|x| x + 1);
}
