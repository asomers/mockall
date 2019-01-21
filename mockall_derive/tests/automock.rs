// vim: tw=80
//! Integration tests for #[automock]

use mockall_derive::*;
use std::default::Default;

// automatic-style mocking with associated types
#[test]
fn associated_types_auto() {
    #[automock(type T=u32;)]
    trait A {
        type T: Clone;
        fn foo(&self, x: Self::T) -> Self::T;
    }

    let mut mock = MockA::default();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(4, mock.foo(4));
}

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
fn generic_struct() {
    #[allow(unused)]
    struct GenericStruct<'a, T, V> {
        t: T,
        v: &'a V
    }
    #[automock]
    impl<'a, T, V> GenericStruct<'a, T, V> {
        #[allow(unused)]
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
fn generic_struct_with_bounds() {
    #[allow(unused)]
    struct GenericStruct<'a, T: Copy, V: Clone> {
        t: T,
        v: &'a V
    }
    #[automock]
    impl<'a, T: Copy, V: Clone> GenericStruct<'a, T, V> {
        #[allow(unused)]
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
fn generic_trait_with_bounds() {
    #[automock]
    trait A<T: Copy> {
        fn foo(&self);
    }

    let mut mock = MockA::<u32>::default();
    mock.expect_foo()
        .returning(|_| ());
    mock.foo();
}

#[test]
fn impl_generic_trait() {
    trait Foo<T> {
        fn foo(&self, t: T) -> T;
    }

    #[allow(unused)]
    struct SomeStruct<T> {
        _t: std::marker::PhantomData<T>
    }

    #[automock]
    impl<T> Foo<T> for SomeStruct<T> {
        fn foo(&self, t: T) -> T {
            t
        }
    }

    let mut mock = MockSomeStruct::<u32>::default();
    mock.expect_foo()
        .returning(|t| t);
    assert_eq!(4, <MockSomeStruct<u32> as Foo<u32>>::foo(&mock, 4));
}

#[test]
fn impl_trait() {
    trait Foo {
        fn foo(&self, x: u32) -> i64;
    }

    #[allow(unused)]
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
    assert_eq!(5, <MockSomeStruct as Foo>::foo(&mock, 4));
}

#[test]
fn impl_trait_on_generic() {
    trait Foo {
        fn foo(&self, x: u32) -> i64;
    }

    #[allow(unused)]
    struct SomeStruct<T> {
        _t: std::marker::PhantomData<T>
    }

    #[automock]
    impl<T> Foo for SomeStruct<T> {
        fn foo(&self, _x: u32) -> i64 {
            42
        }
    }

    let mut mock = MockSomeStruct::<u32>::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

/// mockall should be able to mock methods with at least 16 arguments
#[test]
#[allow(unused)]
fn many_args() {
    #[automock]
    trait ManyArgs {
        fn foo(&self, _a0: u8, _a1: u8, _a2: u8, _a3: u8, _a4: u8, _a5: u8,
               _a6: u8, _a7: u8, _a8: u8, _a9: u8, _a10: u8, _a11: u8,
               _a12: u8, _a13: u8, _a14: u8, _a15: u8);
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
    trait MethodByValue {
        fn foo(self, _x: u32) -> i64;
    }

    let mut mock = MockMethodByValue::default();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
fn reference_return() {
    #[automock]
    trait A<'a> {
        fn foo(&self) -> &'a u32;
    }

    const X: u32 = 5;
    let mut mock = MockA::default();
    mock.expect_foo()
        .returning(|_| &X);
    assert_eq!(5, *mock.foo());
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
    struct SimpleStruct {
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
