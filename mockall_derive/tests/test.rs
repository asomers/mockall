// vim: tw=80

use mockall;
use mockall_derive::*;
use std::default::Default;

/// Mocking a trait with associated types currently requires manual mocking.
/// TODO: add derive support for this, perhaps by providing the "type T=u32" as
/// an attr argument to mock.
#[test]
fn associated_types() {
    trait A {
        type T;
        fn foo(&self, x: Self::T) -> Self::T;
    }

    #[derive(Default)]
    struct MockA {
        foo: ::mockall::Expectation<<MockA as A>::T, <MockA as A>::T>,
    }
    impl A for MockA {
        type T=u32;

        fn foo(&self, x: Self::T) -> Self::T {
            self.foo.call(x)
        }
    }
    impl MockA {
        pub fn expect_foo(&mut self)
            -> &mut ::mockall::Expectation<<Self as A>::T, <Self as A>::T>
        {
            &mut self.foo
        }
    }

    let mut mock = MockA::default();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(4, mock.foo(4));
}

// automatic-style mocking with associated types
//#[test]
//fn associated_types_auto() {
    //#[automock(type T=u32)]
    //trait A {
        //type T;
        //fn foo(&self, x: Self::T) -> Self::T;
    //}

    //let mut mock = MockA::default();
    //mock.expect_foo()
        //.returning(|x| x);
    //assert_eq!(4, mock.foo(4));
//}

#[test]
fn consume_parameters() {
    struct NonCopy{}
    #[automock]
    trait T {
        fn foo(&self, x: NonCopy);
    }

    let mut mock = MockT::default();
    mock.expect_foo()
        .returning(|_x: NonCopy| ());
    mock.foo(NonCopy{});
}

/// Mock a struct whose definition is inaccessible
mod external_struct {
    use super::*;

    // A struct with a definition like this:
    // struct ExternalStruct {
    //     _x: i16
    // }
    // impl ExternalStruct {
    //     fn foo(&self, _x: u32) -> u32 {
    //         42
    //     }
    // }
    // Could be mocked like this:
    mock!{
        ExternalStruct {
            fn foo(&self, x: u32) -> u32;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExternalStruct::default();
        mock.expect_foo()
            .returning(|x| x + 1);
        assert_eq!(6, mock.foo(5));
    }
}

#[test]
fn generic_parameters() {
    #[automock]
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
fn generic_return() {
    #[automock]
    trait A {
        fn foo<T: 'static>(&self, t: T) -> T;
    }

    let mut mock = MockA::default();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| 42u32);
    mock.expect_foo::<i16>()
        .returning(|_x: i16| 42i16);
    assert_eq!(42u32, mock.foo(5u32));
    assert_eq!(42i16, mock.foo(-1i16));
}

#[test]
#[allow(unused)]
fn generic_struct() {
    #[automock]
    struct GenericStruct<'a, T, V> {
        t: T,
        v: &'a V
    }
    #[automock]
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
fn generic_trait() {
    #[automock]
    trait A<T> {
        fn foo(&self);
    }

    let mut mock = MockA::<u32>::default();
    mock.expect_foo()
        .returning(|_| ());
    mock.foo();
}

#[test]
#[allow(unused)]
fn impl_trait() {
    trait Foo {
        fn foo(&self, x: u32) -> i64;
    }

    #[automock]
    struct SomeStruct {}

    #[automock]
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

//mod inherited_trait {
    //use super::*;

    //trait B {
        //fn foo(&self);
    //}

    //trait A: B {
        //fn bar(&self);
    //}

    //mock!{
        //MockA,
        //trait A {
            //fn bar(&self);
        //},
        //trait B {
            //fn foo(&self);
        //}
    //}

    //#[test]
    //fn t() {
        //let mut mock = MockA::default();
        //mock.expect_foo().returning(|| ());
        //mock.expect_bar().returning(|| ());
        //mock.foo();
        //mock.bar();
    //}
//}

/// mockall should be able to mock methods with at least 16 arguments
#[test]
#[allow(unused)]
fn many_args() {
    #[automock]
    struct ManyArgs {}
    #[automock]
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
    #[automock]
    struct MethodByValue {}

    #[automock]
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
fn multi_trait() {
    trait A {}
    trait B {}
    #[automock]
    struct MultiTrait {}
    #[automock]
    impl A for MultiTrait {}
    #[automock]
    impl B for MultiTrait {}

    fn foo<T: A + B>(_t: T) {}

    let mock = MockMultiTrait::default();
    foo(mock);
}

#[test]
#[allow(unused)]
fn pub_crate_struct() {
    #[automock]
    pub(crate) struct PubStruct {
        x: i16
    }
    #[automock]
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
        #[automock]
        pub(super) struct PubStruct {
            x: i16
        }
        #[automock]
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
    #[automock]
    pub struct PubStruct {
        x: i16
    }
    #[automock]
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

// TODO: mock non-'static lifetimes
//#[test]
//fn return_lifetime() {
    //#[automock]
    //trait A<'a> {
        //fn foo(&'a self) -> &'a u32;
    //}

    //let mut mock = MockA::<'static>::default();
    //mock.expect_foo()
        //.returning(|_| &5);
    //assert_eq!(5, *mock.foo());
//}

#[test]
fn return_owned() {
    struct NonCopy{}
    #[automock]
    trait T {
        fn foo(&self) -> NonCopy;
    }

    let mut mock = MockT::default();
    let r = NonCopy{};
    mock.expect_foo()
        .return_once(|_| r);
    mock.foo();
}

// TODO: mock non-'static lifetimes
///// Mock a method that returns through its arguments
//#[test]
//fn return_parameters() {
    //#[automock]
    //trait T {
        //fn foo(&self, x: &mut u32);
    //}

    //let mut mock = MockT::default();
    //let mut x = 5;
    //mock.expect_foo()
        //.returning(|x: &mut u32| {
            //*x = 42;
        //});
    //mock.foo(&mut x);
    //assert_eq!(42, x);
//}

#[test]
fn send() {
    #[automock]
    trait T {
        fn foo(&self) -> u32;
    }

    let mock = MockT::default();
    Box::new(mock) as Box<T + Send>;
}

#[test]
#[allow(unused)]
fn simple_struct() {
    #[automock]
    struct SimpleStruct {
        x: i16
    }
    #[automock]
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
    #[automock]
    trait SimpleTrait {
        fn foo(&self, x: u32) -> u32;
    }

    let mut mock = MockSimpleTrait::default();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}

/// Traits with static methods may be mocked, even if expectations can't be set
/// on the static method
#[test]
fn static_method() {
    #[automock]
    trait A {
        fn bar() -> u32;
        fn foo(&self, x: u32) -> u32;
    }

    let mut mock = MockA::default();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}
