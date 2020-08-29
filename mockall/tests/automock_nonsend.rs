// vim: tw=80
//! A method may have non-Send arguments and/or return values.
#![deny(warnings)]

use mockall::*;
// Rc is not Send
use std::rc::Rc;

// Neither Send nor Clone
pub struct NonSend(Rc<u32>);

mod normal_method {
    use super::*;

    #[automock]
    trait Foo {
        fn foo(&self, x: Rc<u32>);
        fn bar(&self) -> Rc<u32>;
        fn baz(&self) -> NonSend;
    }

    #[test]
    fn return_once_st() {
        let mut mock = MockFoo::new();
        let r = NonSend(Rc::new(42u32));
        mock.expect_baz()
            .return_once_st(move || r);
        assert_eq!(42, *mock.baz().0);
    }

    #[test]
    fn return_const_st() {
        let mut mock = MockFoo::new();
        mock.expect_bar()
            .return_const_st(Rc::new(43u32));
        assert_eq!(43, *mock.bar());
    }

    #[test]
    fn returning_st() {
        let mut mock = MockFoo::new();
        mock.expect_bar()
            .returning_st(|| Rc::new(43u32));
        assert_eq!(43, *mock.bar());
    }

    #[test]
    fn withf_st() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .withf_st(|x| **x == 42)
            .return_const(());
        mock.foo(Rc::new(42));
    }
}

mod ref_method {
    // ref methods don't have return_once_st because they don't return owned
    // values, and they don't have returning_st because they don't have
    // returning.  Instead, they only have return_const, which does not require
    // Send.
    // fn foo(&self) -> &Rc<u32>;
}

mod refmut_method {
    use super::*;

    #[automock]
    trait Foo {
        fn foo(&mut self, x: Rc<u32>);
        fn bar(&mut self) -> &mut Rc<u32>;
    }

    // refmut methods don't have return_once_st because they don't return owned
    // values.
    #[test]
    fn returning_st() {
        let mut mock = MockFoo::new();
        mock.expect_bar()
            .returning_st(|| Rc::new(43u32));
        assert_eq!(43, **mock.bar());
    }

    #[test]
    fn withf_st() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .withf_st(|x| **x == 42)
            .return_const(());
        mock.foo(Rc::new(42));
    }
}

pub mod static_method {
    use super::*;
    use std::sync::Mutex;

    lazy_static! {
        static ref FOO_MTX: Mutex<()> = Mutex::new(());
        static ref BAR_MTX: Mutex<()> = Mutex::new(());
        static ref BAZ_MTX: Mutex<()> = Mutex::new(());
    }

    #[automock]
    trait Foo {
        fn foo(x: Rc<u32>);
        fn bar() -> Rc<u32>;
        fn baz() -> NonSend;
    }

    #[test]
    fn return_once_st() {
        let _guard = BAZ_MTX.lock().unwrap();
        let ctx = MockFoo::baz_context();
        let r = NonSend(Rc::new(42u32));
        ctx.expect()
            .return_once_st(move || r);
        assert_eq!(42, *MockFoo::baz().0);
    }

    #[test]
    fn returning_st() {
        let _guard = BAR_MTX.lock().unwrap();
        let ctx = MockFoo::bar_context();
        ctx.expect()
            .returning_st(|| Rc::new(42));
        assert_eq!(42, *MockFoo::bar());
    }

    #[test]
    fn return_const_st() {
        let _guard = BAR_MTX.lock().unwrap();
        let ctx = MockFoo::bar_context();
        ctx.expect()
            .return_const_st(Rc::new(42));
        assert_eq!(42, *MockFoo::bar());
    }

    #[test]
    fn withf_st() {
        let _guard = FOO_MTX.lock().unwrap();
        let ctx = MockFoo::foo_context();
        ctx.expect()
            .withf_st(|x| **x == 42)
            .return_const(());
        MockFoo::foo(Rc::new(42));
    }
}
