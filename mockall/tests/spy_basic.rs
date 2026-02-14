// vim: tw=80
//! Basic spy tests: delegation, partial mock, calling_real(), checkpoint
#![deny(warnings)]

use mockall::*;

struct RealFoo;

trait MyTrait {
    fn method(&self, x: u32) -> u32;
    fn other(&mut self, s: &str) -> bool;
}

impl MyTrait for RealFoo {
    fn method(&self, x: u32) -> u32 {
        x * 2
    }
    fn other(&mut self, _s: &str) -> bool {
        false
    }
}

spy! {
    Foo {
    }
    impl MyTrait for Foo {
        fn method(&self, x: u32) -> u32;
        fn other(&mut self, s: &str) -> bool;
    }
}

mod delegation {
    use super::*;

    #[test]
    fn delegates_to_real_by_default() {
        let real = RealFoo;
        let spy = SpyFoo::new(real);
        assert_eq!(spy.method(21), 42);
    }

    #[test]
    fn delegates_mut_method() {
        let real = RealFoo;
        let mut spy = SpyFoo::new(real);
        assert!(!spy.other("test"));
    }
}

mod partial_mock {
    use super::*;

    #[test]
    fn override_one_method() {
        let real = RealFoo;
        let mut spy = SpyFoo::new(real);
        spy.expect_other()
            .returning(|_| true);
        // overridden method returns mock value
        assert!(spy.other("test"));
        // non-overridden method still delegates
        assert_eq!(spy.method(10), 20);
    }
}

mod calling_real {
    use super::*;

    #[test]
    fn calling_real_delegates_and_records() {
        let real = RealFoo;
        let mut spy = SpyFoo::new(real);
        spy.expect_method()
            .times(1)
            .calling_real();
        // Should delegate to real AND record the call
        assert_eq!(spy.method(5), 10);
    }
}

mod into_spy {
    use super::*;
    use mockall::IntoSpy;

    #[test]
    fn into_spy_delegates_by_default() {
        let real = RealFoo;
        let spy: SpyFoo<_> = real.into_spy();
        assert_eq!(spy.method(21), 42);
    }

    #[test]
    fn into_spy_with_expectation() {
        let real = RealFoo;
        let mut spy: SpyFoo<_> = real.into_spy();
        spy.expect_method()
            .returning(|x| x + 1);
        assert_eq!(spy.method(5), 6);
    }
}

mod checkpoint {
    use super::*;

    #[test]
    fn checkpoint_resets_expectations() {
        let real = RealFoo;
        let mut spy = SpyFoo::new(real);
        spy.expect_method()
            .returning(|_| 999);
        assert_eq!(spy.method(1), 999);
        spy.checkpoint();
        // After checkpoint, should delegate to real again
        assert_eq!(spy.method(5), 10);
    }
}
