// vim: tw=80
//! Mocking an entire module of functions
#![deny(warnings)]

mod m {
    use mockall::*;
    type T = u32;

    #[automock]
    #[allow(unused)]
    mod foo {
        use super::T;
        pub fn bar(_x: T) -> i64 {unimplemented!()}
        // We must have a separate method for every should_panic test
        pub fn bar1(_x: T) -> i64 {unimplemented!()}
        // Module functions should be able to use impl Trait, too
        pub fn baz() -> impl std::fmt::Debug + Send { unimplemented!()}
        // Module functions can use mutable arguments
        pub fn bean(mut x: u32) { unimplemented!() }
    }

    #[test]
    #[should_panic(expected = "mock_foo::bar1: No matching expectation found")]
    fn with_no_matches() {
        let ctx = mock_foo::bar1_context();
        ctx.expect()
            .with(predicate::eq(4))
            .return_const(0);
        mock_foo::bar1(5);
    }

    #[test]
    fn returning() {
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
