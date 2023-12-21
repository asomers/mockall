// vim: tw=80
//! It should be possible to mock things that use raw identifiers
#![deny(warnings)]
#![allow(non_camel_case_types)]

use mockall::*;

#[automock]
trait r#while {
    fn r#match(&self);
    fn r#loop();
}

#[automock]
pub mod r#break {
    pub fn r#if() {unimplemented!() }
}

mock! {
    r#do {}
    impl r#while for r#do {
        fn r#match(&self);
        fn r#loop();
    }
}

struct r#else {}
#[automock]
impl r#while for r#else {
    fn r#match(&self) {unimplemented!()}
    fn r#loop() {unimplemented!()}
}

#[test]
fn by_ref() {
    let mut foo = Mockwhile::new();
    foo.expect_match()
        .return_const(());
    foo.r#match();
}

#[test]
fn static_method() {
    let ctx = Mockwhile::loop_context();
    ctx.expect()
        .returning(|| ());
    Mockwhile::r#loop();
}

#[test]
fn manual_mock() {
    let mut foo = Mockdo::new();
    foo.expect_match()
        .return_const(());
    foo.r#match();
}

#[test]
fn module() {
    let ctx = mock_break::if_context();
    ctx.expect()
        .returning(|| ());
    mock_break::r#if();
}

#[test]
fn trait_impl() {
    let mut mock = Mockelse::new();
    mock.expect_match()
        .returning(|| ());
    mock.r#match();
}
