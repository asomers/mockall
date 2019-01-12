// vim: tw=80

use mockall;
use mockall_derive::mock;
use std::default::Default;

/// Mocking a trait with associated types currently requires manual mocking.
/// TODO: add derive support for this.
#[test]
fn associated_types() {
    trait A {
        type T;
        fn foo(&self, x: Self::T) -> Self::T;
    }

    #[derive(Default)]
    struct MockA {
        e: ::mockall::Expectations,
    }
    impl A for MockA {
        type T=u32;

        fn foo(&self, x: Self::T) -> Self::T {
            self.e.called::<Self::T, Self::T>("foo", x)
        }
    }
    impl MockA {
        pub fn expect_foo(&mut self)
            -> ::mockall::ExpectationBuilder<<Self as A>::T, <Self as A>::T>
        {
            self.e.expect::<<Self as A>::T, <Self as A>::T>("foo")
        }
    }

    let mut mock = MockA::default();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(4, mock.foo(4));
}

#[test]
fn consume_parameters() {
    struct NonCopy{}
    #[mock]
    trait T {
        fn foo(&self, x: NonCopy);
    }

    let mut mock = MockT::default();
    mock.expect_foo()
        .returning(|_x: NonCopy| ());
    mock.foo(NonCopy{});
}

#[test]
fn generic_parameters() {
    #[mock]
    trait A {
        fn foo<T: 'static>(&self, t: T);
    }

    let mut mock = MockA::default();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| ());
    mock.expect_foo::<i16>()
        .returning(|_x: i16| ());
    mock.foo(5u32);
    mock.foo(-1i16);
}

#[test]
#[allow(unused)]
fn generic_struct() {
    #[mock]
    struct GenericStruct<'a, T, V> {
        t: T,
        v: &'a V
    }
    #[mock]
    impl<'a, T, V> GenericStruct<'a, T, V> {
        fn foo(&self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockGenericStruct::<'static, u8, i8>::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
#[allow(unused)]
fn impl_trait() {
    trait Foo {
        fn foo(&self, x: u32) -> i64;
    }

    #[mock]
    struct SomeStruct {}

    #[mock]
    impl Foo for SomeStruct {
        fn foo(&self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockSomeStruct::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

/// mockall should be able to mock methods with at least 16 arguments
#[test]
#[allow(unused)]
fn many_args() {
    #[mock]
    struct ManyArgs {}
    #[mock]
    impl ManyArgs {
        fn foo(&self, _a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
               _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
               _a12: u8, _a13: u8, _a14: u8, _a15: u8) {
        }
    }

    let mut mock = MockManyArgs::default();
    mock.expect_foo()
        .returning(|_: (u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8, u8)| ());
    mock.foo(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#[test]
#[allow(unused)]
fn method_self_by_value() {
    #[mock]
    struct MethodByValue {}

    #[mock]
    impl MethodByValue {
        fn foo(self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockMethodByValue::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
#[allow(unused)]
fn pub_crate_struct() {
    #[mock]
    pub(crate) struct PubStruct {
        x: i16
    }
    #[mock]
    impl PubStruct {
        pub(crate) fn foo(&self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockPubStruct::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
#[allow(unused)]
fn pub_super_struct() {
    mod m {
        use super::*;
        #[mock]
        pub(super) struct PubStruct {
            x: i16
        }
        #[mock]
        impl PubStruct {
            pub(super) fn foo(&self, _x: u32) -> i64 {
                42
            }
        }
    }

    let mut mock = m::MockPubStruct::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
#[allow(unused)]
fn pub_struct() {
    #[mock]
    pub struct PubStruct {
        x: i16
    }
    #[mock]
    impl PubStruct {
        pub fn foo(&self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockPubStruct::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
#[allow(unused)]
fn simple_struct() {
    #[mock]
    struct SimpleStruct {
        x: i16
    }
    #[mock]
    impl SimpleStruct {
        fn foo(&self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockSimpleStruct::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
fn simple_trait() {
    #[mock]
    trait SimpleTrait {
        fn foo(&self, x: u32) -> u32;
    }

    let mut mock = MockSimpleTrait::default();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}
