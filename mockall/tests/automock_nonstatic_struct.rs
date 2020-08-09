// vim: tw=80
//! Mock a struct with a lifetime parameter
#![deny(warnings)]

use mockall::*;

pub struct NonStaticStruct<'nss> {
    _x: &'nss i32
}

#[automock]
impl<'nss> NonStaticStruct<'nss> {
    pub fn foo(&self) -> i64 {
        42
    }
    pub fn bar() -> i64{
        42
    }
    // XXX Constructors aren't yet supported for non-static Structs
    //pub fn new(x: &'nss i32) -> Self {
        //NonStaticStruct{x}
    //}
}

#[test]
fn normal_method() {
    // This function serves to define a named lifetime
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn has_lt<'a>(_x: &'a i8) {
        let mut mock = MockNonStaticStruct::<'a>::default();
        mock.expect_foo()
            .returning(|| 5);
        assert_eq!(5, mock.foo());
    }

    let x = 42i8;
    has_lt(&x);
}

#[test]
fn static_method() {
    // This function serves to define a named lifetime
    #[allow(clippy::trivially_copy_pass_by_ref)]
    fn has_lt<'a>(_x: &'a i8) {
        let ctx = MockNonStaticStruct::<'a>::bar_context();
        ctx.expect()
            .returning(|| 5);
        assert_eq!(5, MockNonStaticStruct::bar());
    }

    let x = 42i8;
    has_lt(&x);
}
