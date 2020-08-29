// vim: tw=80
//! A method that returns a type which is a common target for std::ops::Deref
#![deny(warnings)]

use mockall::*;
use std::{
    ffi::{CStr, CString, OsStr, OsString},
    path::{Path, PathBuf},
};

#[automock]
trait Foo {
    fn name(&self) -> &CStr;
    fn alias(&self) -> &str;
    fn desc(&self) -> &OsStr;
    fn path(&self) -> &Path;
    fn text(&self) -> &'static str;
    fn slice(&self) -> &[i32];
}

mod return_const {
    use super::*;

    #[test]
    fn cstr() {
        let mut mock = MockFoo::new();
        let name = CString::new("abcd").unwrap();
        mock.expect_name().return_const(name.clone());
        assert_eq!(name.as_c_str(), mock.name());
    }

    #[test]
    fn osstr() {
        let mut mock = MockFoo::new();
        let desc = OsString::from("abcd");
        mock.expect_desc().return_const(desc.clone());
        assert_eq!(desc.as_os_str(), mock.desc());
    }

    #[test]
    fn path() {
        let mut mock = MockFoo::new();
        let mut pb = PathBuf::new();
        pb.push("foo");
        pb.push("bar");
        pb.push("baz");
        mock.expect_path().return_const(pb.clone());
        assert_eq!(pb.as_path(), mock.path());
    }

    #[test]
    fn str() {
        let mut mock = MockFoo::new();
        mock.expect_alias().return_const("abcd".to_owned());
        assert_eq!("abcd", mock.alias());
    }

    #[allow(clippy::redundant_static_lifetimes)]
    #[test]
    fn static_str() {
        const TEXT: &'static str = "abcd";
        let mut mock = MockFoo::new();
        mock.expect_text().return_const(TEXT);
        assert_eq!("abcd", mock.text());
    }

    #[test]
    fn slice() {
        let r = vec![1, 2, 3];
        let mut mock = MockFoo::new();
        mock.expect_slice().return_const(r);
        assert_eq!(&[1, 2, 3], mock.slice());
    }
}
