// vim: tw=80
//! Basic autospy tests: delegation, partial mock, IntoSpy
#![deny(warnings)]

use mockall::*;

#[autospy]
trait MyTrait {
    fn method(&self, x: u32) -> u32;
    fn other(&mut self, s: &str) -> bool;
}

struct RealFoo;

impl MyTrait for RealFoo {
    fn method(&self, x: u32) -> u32 {
        x * 2
    }
    fn other(&mut self, _s: &str) -> bool {
        false
    }
}

#[test]
fn delegates_to_real_by_default() {
    let real = RealFoo;
    let spy = SpyMyTrait::new(real);
    assert_eq!(spy.method(21), 42);
}

#[test]
fn override_with_expectation() {
    let real = RealFoo;
    let mut spy = SpyMyTrait::new(real);
    spy.expect_method()
        .returning(|x| x + 1);
    assert_eq!(spy.method(5), 6);
}

#[test]
fn mut_method_delegates() {
    let real = RealFoo;
    let mut spy = SpyMyTrait::new(real);
    assert!(!spy.other("hello"));
}

#[test]
fn into_spy_works() {
    use mockall::IntoSpy;
    let real = RealFoo;
    let spy: SpyMyTrait<_> = real.into_spy();
    assert_eq!(spy.method(10), 20);
}
