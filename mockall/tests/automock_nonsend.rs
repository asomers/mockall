// vim: tw=80
//! A method may have non-Send arguments and/or return values

use mockall::*;
use std::rc::Rc;

#[automock]
trait Foo {
    // Rc is not Send
    fn foo(&self, x: Rc<u32>) -> Rc<u32>;
}

#[test]
fn returning_st() {
    let mut mock = MockFoo::new();
    let y = Rc::new(43u32);
    mock.expect_foo()
        .returning_st(move |_| y.clone());
    let x = Rc::new(42u32);
    assert_eq!(43, *mock.foo(x).as_ref());
}

#[test]
fn withf_st() {
    let mut mock = MockFoo::new();
    let x = Rc::new(42u32);
    let argument = x.clone();
    mock.expect_foo()
        .withf_st(move |x| *x == argument)
        .returning_st(|_| Rc::new(43u32));
    assert_eq!(43, *mock.foo(x).as_ref());
}
