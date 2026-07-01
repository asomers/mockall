// vim: tw=80
//! Tests for spy with multiple trait implementations
#![deny(warnings)]

use mockall::*;

trait TraitA {
    fn alpha(&self, x: i32) -> i32;
}

trait TraitB {
    fn beta(&self) -> String;
}

struct RealBar;

impl TraitA for RealBar {
    fn alpha(&self, x: i32) -> i32 {
        x + 100
    }
}

impl TraitB for RealBar {
    fn beta(&self) -> String {
        "real".to_string()
    }
}

spy! {
    Bar {
    }
    impl TraitA for Bar {
        fn alpha(&self, x: i32) -> i32;
    }
    impl TraitB for Bar {
        fn beta(&self) -> String;
    }
}

#[test]
fn delegates_both_traits() {
    let real = RealBar;
    let spy = SpyBar::new(real);
    assert_eq!(spy.alpha(1), 101);
    assert_eq!(spy.beta(), "real");
}

#[test]
fn override_one_trait_delegate_other() {
    let real = RealBar;
    let mut spy = SpyBar::new(real);
    spy.expect_beta()
        .returning(|| "mocked".to_string());
    // TraitB overridden
    assert_eq!(spy.beta(), "mocked");
    // TraitA still delegates
    assert_eq!(spy.alpha(5), 105);
}

#[test]
fn into_spy_delegates_both_traits() {
    use mockall::IntoSpy;
    let real = RealBar;
    let spy: SpyBar<_> = real.into_spy();
    assert_eq!(spy.alpha(1), 101);
    assert_eq!(spy.beta(), "real");
}

#[test]
fn override_both_traits() {
    let real = RealBar;
    let mut spy = SpyBar::new(real);
    spy.expect_alpha()
        .returning(|_| -1);
    spy.expect_beta()
        .returning(|| "fake".to_string());
    assert_eq!(spy.alpha(42), -1);
    assert_eq!(spy.beta(), "fake");
}
