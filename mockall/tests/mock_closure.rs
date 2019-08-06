// vim: tw=80
//! A method with a closure argument can be mocked, by turning the closure into
//! a Boxed Fn.

use mockall::*;

struct NonCopy(u32);

mock!{
    Foo {
        fn foo<F: Fn(u32) -> u32 + 'static>(&self, f: F) -> u32;
        fn bar<F: FnMut(u32) -> u32 + 'static>(&self, f: F) -> u32;
        fn baz<F: FnOnce(u32) -> NonCopy + 'static>(&self, f: F) -> NonCopy;
        fn bean<F: Fn(u32) -> u32 + 'static>(f: F) -> u32;
    }
}

mod returning {
    use super::*;

    #[test]
    fn immutable() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|f| f(42));
        assert_eq!(84, mock.foo(|x| 2 * x));
    }

    #[test]
    fn mutable() {
        let mut mock = MockFoo::new();
        mock.expect_bar()
            .returning(|mut f| {f(42); f(5) } );
        let mut counter = 0;
        assert_eq!(47, mock.bar(move |x| { counter += x; counter } ));
    }

    #[test]
    fn once() {
        let mut mock = MockFoo::new();
        mock.expect_baz()
            .returning(|f| f(42));
        let initial = NonCopy(5);
        assert_eq!(47, mock.baz(move |x| NonCopy(initial.0 + x)).0);
    }

    #[test]
    fn static_method() {
        MockFoo::expect_bean()
            .returning(|f| f(42));
        assert_eq!(84, MockFoo::bean(|x| 2 * x));
    }
}
