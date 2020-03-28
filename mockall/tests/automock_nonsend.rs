// vim: tw=80
//! A method may have non-Send arguments and/or return values

use mockall::*;
use std::rc::Rc;
use std::sync::Mutex;

#[automock]
trait Foo {
    // Rc is not Send
    fn foo(&self, x: Rc<u32>) -> Rc<u32>;

    // Rc is not Send
    fn bar(x: Rc<u32>) -> Rc<u32>;
}

lazy_static! {
    static ref BAR_MTX: Mutex<()> = Mutex::new(());
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
fn returning_st_static() {
    let _m = BAR_MTX.lock().unwrap();

    let mock = MockFoo::bar_context();
    let y = Rc::new(43u32);
    mock.expect()
        .returning_st(move |_| y.clone());
    let x = Rc::new(42u32);
    assert_eq!(43, *MockFoo::bar(x).as_ref());
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

#[test]
fn withf_st_static() {
    let _m = BAR_MTX.lock().unwrap();

    let mock = MockFoo::bar_context();
    let x = Rc::new(42u32);
    let argument = x.clone();
    mock.expect()
        .withf_st(move |x| *x == argument)
        .returning_st(|_| Rc::new(43u32));
    assert_eq!(43, *MockFoo::bar(x).as_ref());
}
