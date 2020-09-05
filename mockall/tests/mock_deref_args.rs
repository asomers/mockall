// vim: tw=80
//! A method whose argument is a common `Deref` implementor
#![deny(warnings)]

use mockall::*;
use std::{
    ffi::{CStr, CString, OsStr, OsString},
    path::{Path, PathBuf},
};

mock! {
    Foo {
        fn foo<T: 'static>(&self, x: Vec<T>);
        fn bar(&self, x: String);
        fn baz(&self, x: CString);
        fn bean(&self, x: OsString);
        fn boom(&self, x: PathBuf);
    }
}

mod with {
    use super::*;

    #[test]
    fn cstr() {
        let mut mock = MockFoo::new();
        mock.expect_baz()
            .with(predicate::eq(CString::new("xxx").unwrap()))
            .return_const(());
        mock.baz(CString::new("xxx").unwrap());
    }

    #[test]
    fn osstr() {
        let mut mock = MockFoo::new();
        mock.expect_bean()
            .with(predicate::eq(OsStr::new("xxx").to_owned()))
            .return_const(());
        mock.bean(OsString::from("xxx"));
    }

    #[test]
    fn path() {
        let mut mock = MockFoo::new();
        mock.expect_boom()
            .with(predicate::eq(Path::new("dir/file").to_owned()))
            .return_const(());
        mock.boom(PathBuf::from("dir/file"));
    }

    #[test]
    fn string() {
        let mut mock = MockFoo::new();
        mock.expect_bar()
            .with(predicate::eq("xxx".to_owned()))
            .return_const(());
        mock.bar(String::from("xxx"));
    }

    #[test]
    fn vec() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i32>()
            .with(predicate::eq(vec![1, 2, 3]))
            .return_const(());
        mock.foo(vec![1, 2, 3]);
    }
}

mod withf {
    use super::*;

    #[test]
    fn cstr() {
        let mut mock = MockFoo::new();
        const WANT: [u8; 4] = [120u8, 120, 120, 0];
        let want = CStr::from_bytes_with_nul(&WANT[..]).unwrap();
        mock.expect_baz()
            .withf(move |s| s.as_c_str() == want)
            .return_const(());
        mock.baz(CString::new("xxx").unwrap());
    }

    #[test]
    fn osstr() {
        let mut mock = MockFoo::new();
        mock.expect_bean()
            .withf(move |s| s.as_os_str() == OsStr::new("xxx"))
            .return_const(());
        mock.bean(OsString::from("xxx"));
    }

    #[test]
    fn path() {
        let mut mock = MockFoo::new();
        mock.expect_boom()
            .withf(move |s| s.as_path() == Path::new("dir/file"))
            .return_const(());
        mock.boom(PathBuf::from("dir/file"));
    }

    #[test]
    fn string() {
        let mut mock = MockFoo::new();
        mock.expect_bar()
            .withf(|sl: &String| sl == "xxx")
            .return_const(());
        mock.bar(String::from("xxx"));
    }

    #[test]
    fn vec() {
        let mut mock = MockFoo::new();
        mock.expect_foo::<i32>()
            .withf(|sl: &Vec<i32>| sl == &[1, 2, 3])
            .return_const(());
        mock.foo(vec![1, 2, 3]);
    }

}
