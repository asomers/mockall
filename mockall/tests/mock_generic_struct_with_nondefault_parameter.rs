// vim: tw=80
//! mock a generic struct and instantiate it with a parameter type that does not
//! implement Default

use mockall::*;

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
#[should_panic(expected =
    "Can only return default values for types that impl std::Default")]
fn return_default() {
    let mut mock = MockExternalStruct::<NonDefault>::new();
    mock.expect_foo();
    mock.foo();
}

#[test]
fn returning() {
    let mut mock = MockExternalStruct::<NonDefault>::new();
    mock.expect_foo()
        .returning(|| NonDefault());
    mock.foo();
}
