// vim: tw=80

use mockall::*;

#[test]
fn match_eq_ok() {
    let mut e = Expectation::<(i32), ()>::default();
    e.returning(|_| ());
    e.with(predicates::ord::eq(5));
    e.call(5);
}

#[test]
#[should_panic]
fn match_eq_fail() {
    let mut e = Expectation::<(i32), ()>::default();
    e.returning(|_| ());
    e.with(predicates::ord::eq(4));
    e.call(5);
}

#[test]
fn match_fn_ok() {
    let mut e = Expectation::<(i32), ()>::default();
    e.returning(|_| ());
    e.with(predicate::function(|x: &i32| *x == 5));
    e.call(5);
}

#[test]
#[should_panic]
fn match_fn_fail() {
    let mut e = Expectation::<(i32), ()>::default();
    e.returning(|_| ());
    e.with(predicate::function(|x: &i32| *x == 6));
    e.call(5);
}

/// A MockObject with a method that takes &mut self like:
/// fn foo(&mut self, x: i32) -> u32
#[test]
fn mutable_self() {
    let mut e = Expectation::<(i32), i32>::default();
    let mut count = 0;
    e.returning(move |x| {
            count += x;
            count
        });
    assert_eq!(5, e.call(5));
    assert_eq!(10, e.call(5));
}

/// A mock struct has two different methods with the same name, from different
/// traits.  Neither automock nor mock! can handle this; manual mocking is
/// required.
///
/// The key is changing the ident names used to store the expectations.  Use
/// "traitname_methodname" as the identifier names, and in the expectation
/// methods' names.
mod name_conflict {
    trait Foo {
        fn meth(&self) -> u32;
    }
    trait Bar {
        fn meth(&self) -> u32;
    }

    #[derive(Default)]
    struct MockA {
        foo_meth: ::mockall::Expectation<(), u32>,
        bar_meth: ::mockall::Expectation<(), u32>,
    }
    impl MockA {
        fn expect_foo_meth(&mut self) -> &mut ::mockall::Expectation<(), u32>
        {
            self.foo_meth = ::mockall::Expectation::new();
            &mut self.foo_meth
        }
        fn expect_bar_meth(&mut self) -> &mut ::mockall::Expectation<(), u32>
        {
            self.bar_meth = ::mockall::Expectation::new();
            &mut self.bar_meth
        }
    }
    impl Foo for MockA {
        fn meth(&self) -> u32 {
            self.foo_meth.call(())
        }
    }
    impl Bar for MockA {
        fn meth(&self) -> u32 {
            self.bar_meth.call(())
        }
    }

    #[test]
    fn t() {
        let mut mock = MockA::default();
        mock.expect_foo_meth().returning(|_| 5);
        mock.expect_bar_meth().returning(|_| 6);
        assert_eq!(5, Foo::meth(&mock));
        assert_eq!(6, Bar::meth(&mock));
    }
}

/// A MockObject with a method that has no arguments or returns
/// fn foo(&self)
#[test]
fn no_args_or_returns() {
    let mut e = Expectation::<(), ()>::default();
    e.returning(|_| ());
    e.call(());
}

/// Expectations return O::default() unless otherwise specified
#[test]
#[cfg_attr(not(feature = "nightly"), ignore)]
fn return_default() {
    let e = Expectation::<(), u32>::default();
    let r = e.call(());
    assert_eq!(u32::default(), r);
}

/// Can't return default for types that aren't Default
#[test]
#[should_panic]
fn return_default_panics_for_non_default_types() {
    struct NonDefault{}
    let e = Expectation::<(), NonDefault>::default();
    e.call(());
}

#[test]
fn return_owned() {
    struct NonCopy{}
    let mut e = Expectation::<(), NonCopy>::default();
    let r = NonCopy{};
    e.return_once(|_| r);
    e.call(());
}

#[test]
#[should_panic(expected = "expected only once")]
fn return_owned_too_many_times() {
    struct NonCopy{}
    let mut e = Expectation::<(), NonCopy>::default();
    let r = NonCopy{};
    e.return_once(|_| r);
    e.call(());
    e.call(());
}

/// A MockObject with a simple method like:
/// fn foo(&self, x: i32) -> u32
#[test]
fn simple_method() {
    let mut e = Expectation::<i32, u32>::default();
    e.returning(|_| 42);
    let r = e.call(5);
    assert_eq!(42, r);
}
