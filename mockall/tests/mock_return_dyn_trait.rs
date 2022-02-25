// vim: tw=80
//! a method that returns a reference to a trait object
#![deny(warnings)]

use mockall::*;

trait Test: Sync {
    fn value(&self) -> i32;
    fn mutate(&mut self);
}

impl Test for i32 {
    fn value(&self) -> i32 {
        *self
    }

    fn mutate(&mut self) {
        *self = 0;
    }
}

mock! {
    Foo {
        fn ref_dyn(&self) -> &dyn Test;
        fn static_dyn(&self) -> &'static dyn Test;
        fn mut_dyn(&mut self) -> &mut dyn Test;
    }
}

#[test]
fn ref_dyn() {
    let mut mock = MockFoo::new();
    mock.expect_ref_dyn()
        .return_const(Box::new(42) as Box<dyn Test>);

    assert_eq!(42, mock.ref_dyn().value());
}

#[test]
fn static_dyn() {
    let mut mock = MockFoo::new();
    mock.expect_static_dyn()
        .return_const(&42 as &'static dyn Test);

    assert_eq!(42, mock.static_dyn().value());
}

#[test]
fn mut_dyn() {
    let mut mock = MockFoo::new();
    mock.expect_mut_dyn()
        .return_var(Box::new(42) as Box<dyn Test>);

    assert_eq!(42, mock.mut_dyn().value());
    mock.mut_dyn().mutate();
    assert_eq!(0, mock.mut_dyn().value());
}
