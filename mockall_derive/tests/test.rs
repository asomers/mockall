// vim: tw=80

use mockall;
use mockall_derive::mock;
use std::default::Default;

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
