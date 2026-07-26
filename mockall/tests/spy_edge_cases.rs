// vim: tw=80
//! Edge case tests for spy: different impl types with same spy
#![deny(warnings)]

use mockall::*;

trait Computable {
    fn compute(&self, x: u32) -> u32;
}

struct Doubler;
impl Computable for Doubler {
    fn compute(&self, x: u32) -> u32 {
        x * 2
    }
}

struct Tripler;
impl Computable for Tripler {
    fn compute(&self, x: u32) -> u32 {
        x * 3
    }
}

spy! {
    Calc {
    }
    impl Computable for Calc {
        fn compute(&self, x: u32) -> u32;
    }
}

#[test]
fn into_spy_with_different_impl_types() {
    use mockall::IntoSpy;
    let spy_doubler: SpyCalc<_> = Doubler.into_spy();
    assert_eq!(spy_doubler.compute(5), 10);

    let spy_tripler: SpyCalc<_> = Tripler.into_spy();
    assert_eq!(spy_tripler.compute(5), 15);
}

#[test]
fn different_impl_types_same_spy() {
    let spy_doubler = SpyCalc::new(Doubler);
    assert_eq!(spy_doubler.compute(5), 10);

    let spy_tripler = SpyCalc::new(Tripler);
    assert_eq!(spy_tripler.compute(5), 15);
}

#[test]
fn override_with_different_impls() {
    let mut spy = SpyCalc::new(Doubler);
    spy.expect_compute()
        .returning(|x| x + 1);
    assert_eq!(spy.compute(5), 6);

    let mut spy2 = SpyCalc::new(Tripler);
    spy2.expect_compute()
        .returning(|x| x + 1);
    assert_eq!(spy2.compute(5), 6);
}
