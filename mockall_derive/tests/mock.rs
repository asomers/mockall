// vim: tw=80
//! Integration tests for mock!{}

use mockall_derive::*;

pub mod checkpoint {
    use super::*;

    // Each checkpoint test must use a separate Mock class, because of the
    // static method.
    macro_rules! mock_foo {
        () => {
            mock!{
                pub Foo {
                    fn bar(&self) -> u32;
                    fn baz() -> u32;
                }
            }
        }
    }

    pub mod ok {
        use super::*;
        mock_foo!{}

        #[test]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|| 5)
                .times_range(1..3);
            mock.bar();
            mock.checkpoint();
        }
    }

    pub mod expect_again {
        use super::*;
        mock_foo!{}

        #[test]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|| 42)
                .times_range(1..3);
            mock.bar();
            mock.checkpoint();

            mock.expect_bar()
                .returning(|| 25);
            assert_eq!(25, mock.bar());
        }
    }

    pub mod not_yet_satisfied {
        use super::*;
        mock_foo!{}

        #[test]
        #[should_panic(expected = "Expectation called fewer than 1 times")]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|| 42)
                .times(1);
            mock.checkpoint();
            panic!("Shouldn't get here!");
        }
    }

    pub mod removes_old_expectations {
        use super::*;
        mock_foo!{}

        #[test]
        #[should_panic(expected = "No matching expectation found")]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|| 42)
                .times_range(1..3);
            mock.bar();
            mock.checkpoint();
            mock.bar();
            panic!("Shouldn't get here!");
        }
    }

    pub mod static_method {
        use super::*;
        mock_foo!{}

        #[test]
        #[should_panic(expected = "Expectation called fewer than 1 times")]
        fn t() {
            let mut mock = MockFoo::new();
            MockFoo::expect_baz()
                .returning(|| 32)
                .times_range(1..3);
            mock.checkpoint();
            panic!("Shouldn't get here!");
        }
    }
}

// Semiautomatic style mocking with associated types
mod associated_types_mock {
    use super::*;

    mock! {
        MyIter {}
        trait Iterator {
            type Item=u32;

            fn next(&mut self) -> Option<u32>;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockMyIter::new();
        mock.expect_next()
            .returning(|| Some(5));
        assert_eq!(5, mock.next().unwrap());
    }
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
        let mut mock = MockExternalStruct::new();
        mock.expect_foo()
            .returning(|x| x + 1);
        assert_eq!(6, mock.foo(5));
    }
}

/// Use mock! to mock a generic struct
// An explicit .clone() is required so as not to return by move
#[allow(clippy::clone_on_copy)]
mod external_generic_struct {
    use super::*;

    // A struct with a definition like this:
    // pub struct ExtGenericStruct<T: Clone> {
    //     _x: i16
    // }
    // impl<T: Clone> ExtGenericStruct<T> {
    //     fn foo(&self, _x: T) -> T {
    //         42
    //     }
    // }
    // Could be mocked like this:
    mock!{
        pub ExtGenericStruct<T: Clone + 'static> {
            fn foo(&self, x: T) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExtGenericStruct::<u32>::new();
        mock.expect_foo()
            .returning(|x| x.clone());
        assert_eq!(5, mock.foo(5));
    }
}

mod external_struct_with_trait {
    use super::*;

    trait Bar {
        fn bar(&self, _x: u32) -> u32;
    }

    // A struct with a definition like this:
    // struct ExternalStruct {
    //     _x: i16
    // }
    // impl ExternalStruct {
    //     fn foo(&self, _x: u32) -> u32 {
    //         42
    //     }
    // }
    // impl Bar for ExternalStruct {
    //     fn bar(&self, _x: u32) -> u32 {
    //         42
    //     }
    // }
    //
    // Could be mocked like this:
    mock!{
        ExternalStruct {
            fn foo(&self, x: u32) -> u32;
        }
        trait Bar {
            fn bar(&self, _x: u32) -> u32;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExternalStruct::new();
        mock.expect_foo()
            .returning(|x| x + 1);
        mock.expect_bar()
            .returning(|x| x - 1);
        assert_eq!(6, mock.foo(5));
        assert_eq!(4, mock.bar(5));
    }
}

mod generic_method_returning_reference {
    use super::*;

    trait Foo {
        fn foo<T: 'static>(&self, t: T) -> &u32;
    }

    mock!{
        MyStruct {}
        trait Foo {
            fn foo<T: 'static>(&self, t: T) -> &u32;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockMyStruct::new();
        mock.expect_foo::<i16>().return_const(5u32);
        assert_eq!(5u32, *mock.foo(99i16));
    }
}

/// mock a generic struct and instantiate it with a parameter type that does not
/// implement Default
mod generic_struct_with_non_default_parameter {
    use super::*;

    struct NonDefault();

    trait Foo<T: 'static> {
        fn foo(&self) -> T;
    }
    mock! {
        ExternalStruct<T: 'static> {}
        trait Foo<T: 'static> {
            fn foo(&self) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExternalStruct::<NonDefault>::new();
        mock.expect_foo().returning(|| NonDefault());
        mock.foo();
    }
}

/// Use mock! to mock a generic struct
mod generic_struct_with_generic_trait {
    use super::*;

    trait Foo<T: 'static> {
        fn foo(&self, x: T) -> T;
    }
    mock! {
        Bar<T: 'static, Z: 'static> {}
        trait Foo<T: 'static> {
            fn foo(&self, x: T) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockBar::<u32, u64>::new();
        mock.expect_foo()
            .returning(|x| x);
        assert_eq!(5u32, mock.foo(5u32));
    }
}

mod generic_struct_with_static_method {
    use super::*;

    // Static methods parameterized on the struct's generic parameter need to be
    // turned into generic methods for mocking.  A struct like this:
    //
    // struct Foo<T> {}
    // impl<T> Foo<T> {
    //     fn foo(t: T) {...}
    // }
    //
    // Can be mocked like this:
    mock! {
        Foo<T: 'static> {
            fn foo<T2: 'static>(t: T2);
        }
    }

    #[test]
    fn t() {
        MockFoo::<u32>::expect_foo::<u32>()
            .returning(|_| ());
        MockFoo::<u32>::foo(42u32);
    }
}

mod generic_struct_with_trait_with_associated_types {
    use super::*;

    mock! {
        Foo<T: 'static> {}
        trait Iterator {
            type Item=T;
            fn next(&mut self) -> Option<T>;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::<u32>::new();
        mock.expect_next()
            .return_const(None);
        assert!(mock.next().is_none());
    }
}

mod impl_trait {
    use std::fmt::Debug;
    use super::*;

    mock!{
        Foo {
            fn foo(&self) -> impl Debug + Send;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::new();
        mock.expect_foo().returning(|| Box::new(4));
        format!("{:?}", mock.foo());
    }
}

mod inherited_trait {
    use super::*;

    trait A {
        fn foo(&self);
    }

    trait B: A {
        fn bar(&self);
    }

    mock!{
        B {}
        trait A {
            fn foo(&self);
        }
        trait B {
            fn bar(&self);
        }
    }

    #[test]
    fn t() {
        let mut mock = MockB::new();
        mock.expect_foo().returning(|| ());
        mock.expect_bar().returning(|| ());
        mock.foo();
        mock.bar();
    }
}

#[allow(unused)]
mod multi_trait {
    use super::*;

    trait A {}
    trait B {}
    mock!{
        MultiTrait {}
        trait A  {}
        trait B  {}
    }

    #[test]
    fn t() {
        fn foo<T: A + B>(_t: T) {}

        let mock = MockMultiTrait::new();
        foo(mock);
    }
}

mod new_method {
    use super::*;

    mock! {
        pub Foo {
            fn foo(&self) -> u32;
            fn new(x: u32) -> Self;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::default();
        mock.expect_foo()
            .returning(|| 42);

        MockFoo::expect_new()
            .return_once(|_| mock);

        let mock = MockFoo::new(5);
        assert_eq!(42, mock.foo());
    }
}

mod reference_arguments {
    use super::*;

    mock!{
        Foo {
            fn foo(&self, x: &u32) -> u32;
        }
    }

    #[test]
    fn t() {
        const Y: u32 = 5;
        let mut mock = MockFoo::new();
        let e = mock.expect_foo()
            .returning(|x| *x);
        e.withf(|x| *x == 5);
        let r = mock.foo(&Y);
        assert_eq!(5, r);
    }
}

mod reference_return {
    use super::*;

    mock! {
        Foo {
            fn foo(&self) -> &u32;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .return_const(5u32);
        assert_eq!(5, *mock.foo());
    }
}

mod ref_mut_return {
    use super::*;

    mock! {
        Foo {
            fn foo(&mut self) -> &mut u32;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .return_var(5u32);
        {
            let r = mock.foo();
            assert_eq!(5, *r);
            *r = 6;
        }
        assert_eq!(6, *mock.foo());
    }
}

mod static_method {
    use super::*;

    mock!{
        Foo {
            fn bar(x: u32) -> u64;
        }
    }

    #[test]
    fn t() {
        MockFoo::expect_bar()
            .returning(|x| u64::from(x + 1));
        assert_eq!(42, MockFoo::bar(41));
    }
}

// TODO: expectation! can't take where clauses.  It would need to be rewritten
// as a proc macro for that.
//mod where_clause_on_static_method {
    //use super::*;

    //mock! {
        //Foo<T: Clone> {
            //fn new<T2>(t: T2) -> MockFoo<T2> where T2: Clone + 'static;
        //}
    //}

    //#[test]
    //fn t() {
        //MockFoo::<u32>::expect_new::<u32>()
            //.returning(|_| MockFoo::default());
        //MockFoo::<u32>::new(42u32);
    //}
//}

// An explicit clone is required so as not to return by move
#[allow(clippy::clone_on_copy)]
mod where_clause_on_struct {
    use super::*;

    mock! {
        Foo<T: 'static> where T:Clone {
            fn foo(&self, t: T) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|t: u32| t.clone());
        assert_eq!(5u32, mock.foo(5u32));
    }
}

// An explicit clone is required so as not to return by move
#[allow(clippy::clone_on_copy)]
mod where_clause_on_struct_with_trait {
    use super::*;

    trait Bar {
        fn bar(&self);
    }
    mock! {
        Foo<T: 'static> where T: Clone {
            fn foo(&self, t: T) -> T;
        }
        trait Bar {
            fn bar(&self);
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .returning(|t: u32| t.clone());
        mock.expect_bar()
            .returning(|| ());
        assert_eq!(5u32, mock.foo(5u32));
        mock.bar();
    }
}
