// vim: tw=80
//! A mock method should be able to return an object parameterized on the
//! anonymous lifetime.
//! https://github.com/asomers/mockall/issues/87
#![deny(warnings)]

use mockall::*;

#[derive(Clone)]
struct X<'a>(&'a u32);

trait T {
    fn trait_method(&self) -> X<'_>;
}

mock! {
    Foo {
        fn inherent_method(&self) -> X<'_>;
    }
    impl T for Foo {
        fn trait_method(&self) -> X<'_>;
    }
}

// It isn't possible to safely set an expectation for a non-'static return value
// (because the mock object doesn't have any lifetime parameters itself), but
// unsafely setting such an expectation is a common use case.
mod return_nonstatic {
    use super::*;

    #[test]
    fn inherent_method() {
        let d = 42u32;
        let x = X(&d);
        let xstatic: X<'static> = unsafe{ std::mem::transmute(x) };
        let mut mock = MockFoo::new();
        mock.expect_inherent_method()
            .returning(move || xstatic.clone());

        assert_eq!(42u32, *mock.inherent_method().0);
    }
    #[test]
    fn trait_method() {
        let d = 42u32;
        let x = X(&d);
        let xstatic: X<'static> = unsafe{ std::mem::transmute(x) };
        let mut mock = MockFoo::new();
        mock.expect_trait_method()
            .returning(move || xstatic.clone());

        assert_eq!(42u32, *mock.trait_method().0);
    }

}
