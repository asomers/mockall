// vim: tw=80
#![deny(warnings)]
use std::fmt::Debug;

use mockall::*;

struct G<T> where T: 'static + ?Sized {t: Box<T>}

mock! {
    Foo {
        fn foo<T>(&self, t: Box<T>) -> G<T> where T: 'static + ?Sized;
        fn bar<T>(&self, g: G<T>) -> Box<T> where T:  'static + ?Sized;
        fn baz<T>(&self) -> &G<T> where T: 'static + ?Sized;
        fn bean<T>(&mut self) -> &mut G<T> where T: 'static + ?Sized;
    }
}

trait SomeTrait: Send + Sync + Debug {
    fn get(&self) -> u32;

    fn add(&mut self, cnt: u32);
}

impl PartialEq for dyn SomeTrait {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

#[derive(Debug, PartialEq, Eq)]
struct SomeStruct {value: u32}

impl SomeTrait for SomeStruct {
    fn get(&self) -> u32 {
        self.value
    }

    fn add(&mut self, cnt: u32) {
        self.value += cnt;
    }
}

#[test]
fn returning() {
    let mut mock = MockFoo::new();
    mock.expect_foo::<dyn SomeTrait>()
        .returning(|t| G { t });
    assert_eq!(
        42,
        mock.foo::<dyn SomeTrait>(Box::new(SomeStruct { value: 42 })).t.get()
    );

    mock.expect_bar::<dyn SomeTrait>()
        .returning(|g| g.t);
    assert_eq!(
        42,
        mock.bar::<dyn SomeTrait>(G{t: Box::new(SomeStruct { value: 42 })}).get()
    );
}

#[test]
fn return_const() {
    let mut mock = MockFoo::new();
    mock.expect_baz::<dyn SomeTrait>()
        .return_const(G{t: Box::new(SomeStruct { value: 42 })});
    assert_eq!(42, mock.baz::<dyn SomeTrait>().t.get());
}

#[test]
fn return_var() {
    let mut mock = MockFoo::new();
    mock.expect_bean::<dyn SomeTrait>()
        .return_var(G{t: Box::new(SomeStruct { value: 42 })});
    mock.bean::<dyn SomeTrait>().t.add(1);
    assert_eq!(43, mock.bean::<dyn SomeTrait>().t.get());
}
