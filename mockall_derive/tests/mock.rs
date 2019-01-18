// vim: tw=80
//! Integration tests for mock!{}

use mockall_derive::*;

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
        let mut mock = MockMyIter::default();
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
        let mut mock = MockExternalStruct::default();
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
        pub ExtGenericStruct<T: Clone + 'static> {
            fn foo(&self, x: T) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExtGenericStruct::<u32>::default();
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
        let mut mock = MockExternalStruct::default();
        mock.expect_foo()
            .returning(|x| x + 1);
        mock.expect_bar()
            .returning(|x| x - 1);
        assert_eq!(6, mock.foo(5));
        assert_eq!(4, mock.bar(5));
    }
}

/// Use mock! to mock a generic struct
mod generic_struct_with_generic_trait {
    use super::*;

    trait Foo<T: 'static> {
        fn foo(&self, x: T) -> T;
    }
    mock! {
        ExternalStruct<T: 'static, Z: 'static> {}
        trait Foo<T: 'static> {
            fn foo(&self, x: T) -> T;
        }
    }

    #[test]
    fn t() {
        let mut mock = MockExternalStruct::<u32, u64>::default();
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
        let mut mock = MockB::default();
        mock.expect_foo().returning(|_| ());
        mock.expect_bar().returning(|_| ());
        mock.foo();
        mock.bar();
    }
}
