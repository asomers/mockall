// vim: tw=80
//! Mocking an entire module of functions
#![deny(warnings)]


pub mod m {
    use std::sync::Mutex;
    use mockall::*;
    type T = u32;

    static BAR_MTX: Mutex<()> = Mutex::new(());

    #[automock]
    pub mod foo {
        use super::T;
        pub fn bar(_x: T) -> i64 {unimplemented!()}
        // Module functions should be able to use impl Trait, too
        pub fn baz() -> impl std::fmt::Debug + Send { unimplemented!()}
        // Module functions can use mutable arguments
        pub fn bean(mut _x: u32) { unimplemented!() }
    }

    #[test]
    #[should_panic(expected = "mock_foo::bar(5): No matching expectation found")]
    fn with_no_matches() {
        let _m = BAR_MTX.lock();
        let ctx = mock_foo::bar_context();
        ctx.expect()
            .with(predicate::eq(4))
            .return_const(0);
        mock_foo::bar(5);
    }

    #[test]
    fn returning() {
        let _m = BAR_MTX.lock();
        let ctx = mock_foo::bar_context();
        ctx.expect()
            .returning(|x| i64::from(x) + 1);
        assert_eq!(5, mock_foo::bar(4));
    }

    #[test]
    fn impl_trait() {
        let ctx = mock_foo::baz_context();
        ctx.expect()
            .returning(|| Box::new(4));
        format!("{:?}", mock_foo::baz());
    }
}
