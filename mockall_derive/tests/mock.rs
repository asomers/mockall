// vim: tw=80
//! Integration tests for mock!{}

use mockall_derive::*;

mod checkpoint {
    use super::*;

    // Each checkpoint test must use a separate Mock class, because of the
    // static method.
    macro_rules! mock_foo {
        () => {
            mock!{
                Foo {
                    fn bar(&self) -> u32;
                    fn baz() -> u32;
                }
            }
        }
    }

    mod ok {
        use super::*;
        mock_foo!{}

        #[test]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|_| 5)
                .times_range(1..3);
            mock.bar();
            mock.checkpoint();
        }
    }

    mod expect_again {
        use super::*;
        mock_foo!{}

        #[test]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|_| 42)
                .times_range(1..3);
            mock.bar();
            mock.checkpoint();

            mock.expect_bar()
                .returning(|_| 25);
            assert_eq!(25, mock.bar());
        }
    }

    mod not_yet_satisfied {
        use super::*;
        mock_foo!{}

        #[test]
        #[should_panic(expected = "Expectation called fewer than 1 times")]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|_| 42)
                .times(1);
            mock.checkpoint();
            panic!("Shouldn't get here!");
        }
    }

    mod removes_old_expectations {
        use super::*;
        mock_foo!{}

        #[test]
        #[should_panic(expected = "No matching expectation found")]
        fn t() {
            let mut mock = MockFoo::new();
            mock.expect_bar()
                .returning(|_| 42)
                .times_range(1..3);
            mock.bar();
            mock.checkpoint();
            mock.bar();
            panic!("Shouldn't get here!");
        }
    }

    mod static_method {
        use super::*;
        mock_foo!{}

        #[test]
        #[should_panic(expected = "Expectation called fewer than 1 times")]
        fn t() {
            let mut mock = MockFoo::new();
            MockFoo::expect_baz()
                .returning(|_| 32)
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
            .returning(|_| Some(5));
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
        pub ExtGenericStruct<T: Clone> {
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

    trait Foo<T> {
        fn foo(&self) -> T;
    }
    mock! {
        ExternalStruct<T> {}
        trait Foo<T> {
            fn foo(&self) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExternalStruct::<NonDefault>::new();
        mock.expect_foo().returning(|_| NonDefault());
        mock.foo();
    }
}

/// Use mock! to mock a generic struct
mod generic_struct_with_generic_trait {
    use super::*;

    trait Foo<T> {
        fn foo(&self, x: T) -> T;
    }
    mock! {
        ExternalStruct<T, Z> {}
        trait Foo<T> {
            fn foo(&self, x: T) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExternalStruct::<u32, u64>::new();
        mock.expect_foo()
            .returning(|x| x);
        assert_eq!(5u32, mock.foo(5u32));
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
        mock.expect_foo().returning(|_| ());
        mock.expect_bar().returning(|_| ());
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
        Foo {
            fn foo(&self) -> u32;
            fn new(x: u32) -> Self;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockFoo::default();
        mock.expect_foo()
            .returning(|_| 42);

        MockFoo::expect_new()
            .return_once(|_| mock);

        let mock = MockFoo::new(5);
        assert_eq!(42, mock.foo());
    }
}

mod reference_arguments {
    use super::*;

    mock!{
        Foo<'a> {
            fn foo(&self, x: &'a u32) -> u32;
        }
    }

    #[test]
    fn t() {
        const Y: u32 = 5;
        let mut mock = MockFoo::new();
        {
            mock.expect_foo().returning(|x| *x);
        }
        {
            let r = mock.foo(&Y);
            assert_eq!(5, r);
        }
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
