// vim: tw=80
//! generic methods whose generic parameters appear in neither inputs nor return
//! types should still be mockable, and it should be possible to set
//! simultaneous expectations for different generic types on the same object.
#![deny(warnings)]

use mockall::*;

pub struct Foo{}

#[automock]
impl Foo {
    #[allow(clippy::extra_unused_type_parameters)]
    pub fn foo<T: 'static>(&self) -> i32 {
        unimplemented!()
    }
    /// A static method
    #[allow(clippy::extra_unused_type_parameters)]
    pub fn bar<T: 'static>() -> i32 {
        unimplemented!()
    }
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<f32>()
        .return_const(42);
    mock.expect_foo::<f64>()
        .return_const(69);
    assert_eq!(42, mock.foo::<f32>());
    assert_eq!(69, mock.foo::<f64>());
}

#[test]
fn static_method() {
    let ctx = MockFoo::bar_context();
    ctx.expect::<f32>()
        .return_const(42);
    ctx.expect::<f64>()
        .return_const(69);
    assert_eq!(42, MockFoo::bar::<f32>());
    assert_eq!(69, MockFoo::bar::<f64>());
}
