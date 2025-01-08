// vim: tw=80
#![deny(warnings)]

pub trait Bar {
    fn foo(&self, x: u32) -> i64;
}

macro_rules! mock_foo {
    ($struct:ident) => {
        mockall::mock! {
            pub $struct {}
            impl $crate::mockall::tests::mock_struct_in_macro::Bar for $struct {
                fn foo(&self, x: u32) -> i64;
            }
        }
    };
}

mock_foo!(Foo);

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo().return_const(6);
    assert_eq!(6, mock.foo(5));
}
