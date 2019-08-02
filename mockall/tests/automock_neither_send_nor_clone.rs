// vim: tw=80
//! A method may have non-Send arguments and/or return values

use mockall::*;
use std::rc::Rc;

pub struct MyType(Rc<u32>);

#[automock]
trait Foo {
    fn foo(&self, x: MyType) -> MyType;
}

#[test]
fn return_once_st() {
    let mut mock = MockFoo::new();
    let y = MyType(Rc::new(43u32));
    mock.expect_foo()
        .return_once_st(move |_| y);
    let x = MyType(Rc::new(42u32));
    assert_eq!(43, *mock.foo(x).0.as_ref());
}
