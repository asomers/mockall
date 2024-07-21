// vim: tw=80
//! #[concretize] should work for closure arguments
#![deny(warnings)]

use mockall::*;

pub struct Foo{}
#[automock]
impl Foo {
    #[concretize]
    pub fn foo<F: Fn(u32) -> u32>(&self, _f: F) -> u32 {todo!()}
    #[concretize]
    pub fn bar<F: FnMut(&mut u32) -> u32>(&self, _f: F) -> u32 {todo!()}
}

#[test]
fn fn_() {
    let mut mock = MockFoo::default();
    mock.expect_foo()
        .returning(|f| f(42));
    assert_eq!(mock.foo(|x| x + 1), 43);
}

#[test]
fn fn_mut() {
    use std::sync::{Arc, Mutex};
    let x = Arc::new(Mutex::new(42u32));
    {
        let mut mock = MockFoo::default();
        let x2 = x.clone();
        mock.expect_bar()
            .returning(move |f| f(&mut x2.lock().unwrap()));
        assert_eq!(mock.bar(|y| {*y += 1; *y}), 43);
    }
    assert_eq!(*x.lock().unwrap(), 43);
}
