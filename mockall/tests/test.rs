// vim: tw=80

use mockall::*;

#[test]
#[should_panic(expected = "No matching expectation found")]
fn missing_expectation() {
    let e = GenericExpectations::default();
    e.call::<i32, u32>(&"foo", 5);
}

/// A MockObject with a method that takes &mut self like:
/// fn foo(&mut self, x: i32) -> u32
#[test]
fn mutable_self() {
    let mut e = GenericExpectations::default();
    let mut count = 0;
    e.expect::<i32, i32>(&"foo")
        .returning(move |x| {
            count += x;
            count
        });
    assert_eq!(5, e.call::<i32, i32>(&"foo", 5));
    assert_eq!(10, e.call::<i32, i32>(&"foo", 5));
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
        e: ::mockall::GenericExpectations,
    }
    impl MockA {
        fn expect_foo_meth(&mut self) -> &mut ::mockall::Expectation<(), u32>
        {
            self.e.expect::<(), u32>("foo_meth")
        }
        fn expect_bar_meth(&mut self) -> &mut ::mockall::Expectation<(), u32>
        {
            self.e.expect::<(), u32>("bar_meth")
        }
    }
    impl Foo for MockA {
        fn meth(&self) -> u32 {
            self.e.call::<(), u32>("foo_meth", ())
        }
    }
    impl Bar for MockA {
        fn meth(&self) -> u32 {
            self.e.call::<(), u32>("bar_meth", ())
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
    let mut e = GenericExpectations::default();
    e.expect::<(), ()>(&"foo")
        .returning(|_| ());
    e.call::<(), ()>(&"foo", ());
}

/// Expectations return O::default() unless otherwise specified
#[test]
#[cfg_attr(not(feature = "nightly"), ignore)]
fn return_default() {
    let mut e = GenericExpectations::default();
    e.expect::<(), u32>(&"foo");
    let r = e.call::<(), u32>(&"foo", ());
    assert_eq!(u32::default(), r);
}

/// Can't return default for types that aren't Default
#[test]
#[should_panic]
fn return_default_panics_for_non_default_types() {
    struct NonDefault{}
    let mut e = GenericExpectations::default();
    e.expect::<(), NonDefault>(&"foo");
    e.call::<(), NonDefault>(&"foo", ());
}

#[test]
fn return_owned() {
    struct NonCopy{}
    let mut e = GenericExpectations::default();
    let r = NonCopy{};
    e.expect::<(), NonCopy>(&"foo")
        .return_once(|_| r);
    e.call::<(), NonCopy>(&"foo", ());
}

#[test]
#[should_panic(expected = "expected only once")]
fn return_owned_too_many_times() {
    struct NonCopy{}
    let mut e = GenericExpectations::default();
    let r = NonCopy{};
    e.expect::<(), NonCopy>(&"foo")
        .return_once(|_| r);
    e.call::<(), NonCopy>(&"foo", ());
    e.call::<(), NonCopy>(&"foo", ());
}

#[test]
fn return_reference() {
    let mut e = RefExpectation::<(), i32>::default();
    e.return_const(5i32);
    assert_eq!(5i32, *e.call(()));
}

#[test]
fn return_mutable_reference() {
    let mut e = RefMutExpectation::<(), i32>::default();
    e.returning(|_| 5i32);
    assert_eq!(5i32, *e.call_mut(()));
}

#[test]
fn return_mutable_reference_return_var() {
    let mut e = RefMutExpectation::<(), i32>::default();
    e.return_var(5i32);
    assert_eq!(5i32, *e.call_mut(()));
}

/// A MockObject with a simple method like:
/// fn foo(&self, x: i32) -> u32
#[test]
fn simple_method() {
    let mut e = GenericExpectations::default();
    e.expect::<i32, u32>(&"foo")
        .returning(|_| 42);
    let r = e.call::<i32, u32>(&"foo", 5);
    assert_eq!(42, r);
}
