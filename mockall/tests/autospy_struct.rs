// vim: tw=80
//! Tests for #[autospy] on struct impl blocks (inherent methods)
#![deny(warnings)]

use mockall::*;

struct Foo;

#[autospy]
impl Foo {
    fn double(&self, x: u32) -> u32 {
        x * 2
    }
    fn is_empty(&self, s: &str) -> bool {
        s.is_empty()
    }
}

#[test]
fn delegates_to_real_by_default() {
    let real = Foo;
    let spy = SpyFoo::new(real);
    assert_eq!(spy.double(21), 42);
}

#[test]
fn override_with_expectation() {
    let real = Foo;
    let mut spy = SpyFoo::new(real);
    spy.expect_double()
        .returning(|x| x + 1);
    assert_eq!(spy.double(5), 6);
}

#[test]
fn second_method_delegates() {
    let real = Foo;
    let spy = SpyFoo::new(real);
    assert!(spy.is_empty(""));
    assert!(!spy.is_empty("hello"));
}

#[test]
fn into_spy_works() {
    use mockall::IntoSpy;
    let real = Foo;
    let spy: SpyFoo = real.into_spy();
    assert_eq!(spy.double(10), 20);
}
