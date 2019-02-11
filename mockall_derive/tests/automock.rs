// vim: tw=80
//! Integration tests for #[automock]

// mocking modules requires the proc_macro_hygiene feature in the _consumer_
// code
#![cfg_attr(feature = "nightly", feature(proc_macro_hygiene))]

use cfg_if::cfg_if;
use mockall_derive::*;
use std::{
    ffi::{CStr, CString, OsStr, OsString},
    fmt::Debug,
    path::{Path, PathBuf}
};

// automatic-style mocking with associated types
#[test]
fn associated_types_auto() {
    #[automock(type T=u32;)]
    trait A {
        type T: Clone;
        fn foo(&self, x: Self::T) -> Self::T;
    }

    let mut mock = MockA::new();
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

    let mut mock = MockT::new();
    mock.expect_foo()
        .returning(|_x: NonCopy| ());
    mock.foo(NonCopy{});
}

#[test]
fn foreign_c() {
    #[automock(mod mock_ffi;)]
    extern "C" {
        #[allow(unused)]
        pub fn foo(x: u32) -> i64;
    }

    mock_ffi::expect_foo().returning(|x| i64::from(x));
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}

#[test]
fn foreign_rust() {
    #[automock(mod mock_ffi;)]
    extern "Rust" {
        #[allow(unused)]
        pub fn foo(x: u32) -> i64;
    }

    mock_ffi::expect_foo().returning(|x| i64::from(x));
    assert_eq!(42, unsafe{mock_ffi::foo(42)});
}

#[test]
fn generic_parameters() {
    #[automock]
    trait A {
        fn foo<T: 'static>(&self, t: T);
    }

    let mut mock = MockA::new();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| ());
    mock.expect_foo::<i16>()
        .returning(|_x: i16| ());
    mock.foo(5u32);
    mock.foo(-1i16);
}

#[test]
fn generic_parameters_returning_ref() {
    #[automock]
    trait A {
        fn foo<T: 'static>(&self, t: T) -> &u32;
    }

    let mut mock = MockA::new();
    mock.expect_foo::<u32>().return_const(5);
    assert_eq!(5, *mock.foo(42u32));
}

#[test]
fn generic_return() {
    #[automock]
    trait A {
        fn foo<T: 'static>(&self, t: T) -> T;
    }

    let mut mock = MockA::new();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| 42u32);
    mock.expect_foo::<i16>()
        .returning(|_x: i16| 42i16);
    assert_eq!(42u32, mock.foo(5u32));
    assert_eq!(42i16, mock.foo(-1i16));
}

#[test]
fn generic_static_method() {
    #[automock]
    trait A {
        fn bar<T: 'static>(t: T) -> u32;
    }

    MockA::expect_bar::<i16>()
        .times(1)
        .returning(|_| 42);
    assert_eq!(42, MockA::bar(-1i16));
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

    let mut mock = MockGenericStruct::<'static, u8, i8>::new();
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

    let mut mock = MockGenericStruct::<'static, u8, i8>::new();
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

    let mut mock = MockA::<u32>::new();
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

    let mut mock = MockA::<u32>::new();
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

    let mut mock = MockSomeStruct::<u32>::new();
    mock.expect_foo()
        .returning(|t| t);
    assert_eq!(4, <MockSomeStruct<u32> as Foo<u32>>::foo(&mock, 4));
}

#[test]
fn impl_trait() {
    #[allow(unused)]
    pub struct Foo {}

    #[automock]
    impl Foo {
        #[allow(unused)]
        fn foo(&self) -> impl Debug + Send { unimplemented!()}
    }

    let mut mock = MockFoo::new();
    mock.expect_foo().returning(|_| Box::new(4));
    format!("{:?}", mock.foo());
}

#[test]
fn impl_trait_on_struct() {
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

    let mut mock = MockSomeStruct::new();
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

    let mut mock = MockSomeStruct::<u32>::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

#[test]
fn impl_trait_with_associated_types() {
    #[allow(unused)]
    struct Foo {}
    #[automock]
    impl Iterator for Foo {
        type Item = u32;

        fn next(&mut self) -> Option<Self::Item> {
            unimplemented!()
        }
    }

    let mut mock = MockFoo::new();
    mock.expect_next().returning(|_| None);
    assert!(mock.next().is_none());
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

    let mut mock = MockManyArgs::new();
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

    let mut mock = MockMethodByValue::new();
    mock.expect_foo()
        .returning(|x| i64::from(x) + 1);
    assert_eq!(5, mock.foo(4));
}

cfg_if! {
    if #[cfg(feature = "nightly")] {
        #[test]
        fn module() {
            #[automock]
            #[allow(unused)]
            mod foo {
                pub fn bar(_x: u32) -> i64 {unimplemented!()}
            }

            mock_foo::expect_bar()
                .returning(|x| i64::from(x) + 1);
            assert_eq!(5, mock_foo::bar(4));
        }
    }
}

/// Structs with a "new" method should mock that method rather than add a new
/// method just for the mock object.
#[test]
#[allow(unused)]
fn new_method() {
    pub struct Foo{}

    #[automock]
    impl Foo {
        pub fn foo(&self) -> i16 {
            unimplemented!()
        }

        pub fn new(_x: u32) -> Self {
            unimplemented!()
        }
    }

    let mut mock = MockFoo::default();
    mock.expect_foo()
        .returning(|_| -1);

    MockFoo::expect_new()
        .return_once(|_| mock);

    let mock = MockFoo::new(5);
    assert_eq!(-1, mock.foo());
}

#[test]
fn reference_return() {
    #[automock]
    trait A {
        fn foo(&self) -> &u32;
    }

    let mut mock = MockA::new();
    mock.expect_foo().return_const(5);
    assert_eq!(5, *mock.foo());
}

#[test]
fn ref_cstr_return() {
    #[automock]
    trait Foo {
        fn name(&self) -> &CStr;
    }

    let mut mock = MockFoo::new();
    let name = CString::new("abcd").unwrap();
    mock.expect_name().return_const(name.clone());
    assert_eq!(name.as_c_str(), mock.name());
}

#[test]
fn ref_osstr_return() {
    #[automock]
    trait Foo {
        fn name(&self) -> &OsStr;
    }

    let mut mock = MockFoo::new();
    let name = OsString::from("abcd");
    mock.expect_name().return_const(name.clone());
    assert_eq!(name.as_os_str(), mock.name());
}

#[test]
fn ref_path_return() {
    #[automock]
    trait Foo {
        fn path(&self) -> &Path;
    }

    let mut mock = MockFoo::new();
    let mut pb = PathBuf::new();
    pb.push("foo");
    pb.push("bar");
    pb.push("baz");
    mock.expect_path().return_const(pb.clone());
    assert_eq!(pb.as_path(), mock.path());
}

#[test]
fn ref_str_return() {
    #[automock]
    trait Foo {
        fn name(&self) -> &str;
    }

    let mut mock = MockFoo::new();
    mock.expect_name().return_const("abcd".to_owned());
    assert_eq!("abcd", mock.name());
}

#[test]
fn ref_mut_return() {
    #[automock]
    trait A {
        fn foo(&mut self) -> &mut u32;
    }

    let mut mock = MockA::new();
    mock.expect_foo().return_var(5);
    {
        let r = mock.foo();
        assert_eq!(5, *r);
        *r = 6;
    }
    assert_eq!(6, *mock.foo());
}

#[test]
fn return_owned() {
    struct NonCopy{}
    #[automock]
    trait T {
        fn foo(&self) -> NonCopy;
    }

    let mut mock = MockT::new();
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

    //let mut mock = MockT::new();
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

    let mock = MockT::new();
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

    let mut mock = MockSimpleStruct::new();
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

    let mut mock = MockSimpleTrait::new();
    mock.expect_foo()
        .returning(|x| x + 1);
    assert_eq!(5, mock.foo(4));
}

/// Traits with static methods may be mocked.
#[test]
fn static_method() {
    #[automock]
    trait A {
        fn bar() -> u32;
        fn foo(&self, x: u32) -> u32;
    }

    MockA::expect_bar()
        .returning(|_| 42);
    assert_eq!(42, MockA::bar());
}

#[test]
fn trait_with_constructor() {
    #[automock]
    trait A {
        fn new() -> Self;
    }

    MockA::expect_new().returning(|_| MockA::default());
    let _a: MockA = <MockA as A>::new();
}

#[test]
fn trait_with_boxed_constructor() {
    #[automock]
    trait A {
        fn new() -> Box<Self>;
    }

    MockA::expect_new().returning(|_| Box::new(MockA::default()));
    let _a: Box<MockA> = <MockA as A>::new();
}

#[test]
fn where_clause_on_method() {
    #[automock]
    trait A {
        fn foo<T>(&self, t: T) where T: 'static;
    }

    let mut mock = MockA::new();
    mock.expect_foo::<u32>()
        .returning(|_x: u32| ());
    mock.expect_foo::<i16>()
        .returning(|_x: i16| ());
    mock.foo(5u32);
    mock.foo(-1i16);
}

#[test]
fn where_clause_on_struct() {
    #[allow(unused)]
    struct GenericStruct<T> {
        t: T,
    }
    #[automock]
    impl<T> GenericStruct<T>
        where T: Clone + Default
    {
        #[allow(unused)]
        fn foo(&self, x: T) -> T {
            x.clone()
        }
    }

    let mut mock = MockGenericStruct::<u8>::default();
    mock.expect_foo()
        .returning(|x| x);
    assert_eq!(4, mock.foo(4u8));
}

#[test]
fn where_clause_on_trait() {
    #[automock]
    trait Foo<T> where T: Clone {
        fn foo(&self);
    }

    let mut mock = MockFoo::<u8>::default();
    mock.expect_foo()
        .returning(|_| ());
    mock.foo();
}
