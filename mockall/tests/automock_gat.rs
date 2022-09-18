#! vim: tw=80
//! automock a trait with Generic Associated Types
#![deny(warnings)]

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "nightly")] {
        use mockall::*;

        // The lifetime must have the same name as in the next() method.
        #[automock(type Item=&'a u32;)]
        trait LendingIterator {
            type Item<'a> where Self: 'a;

            // Clippy doesn't know that Mockall will need the lifetime when it
            // expands the macro.
            #[allow(clippy::needless_lifetimes)]
            fn next<'a>(&'a mut self) -> Option<Self::Item<'a>>;
        }

        // It isn't possible to safely set an expectation for a non-'static
        // return value (because the mock object doesn't have any lifetime
        // parameters itself), but unsafely setting such an expectation is a
        // common use case.
        #[test]
        fn return_const() {
            let mut mock = MockLendingIterator::new();
            let x = 42u32;
            let xstatic: &'static u32 = unsafe{ std::mem::transmute(&x) };
            mock.expect_next().return_const(Some(xstatic));
            assert_eq!(42u32, *mock.next().unwrap());
        }
    }
}
