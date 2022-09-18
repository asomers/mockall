// vim: tw=80
//! Some generic methods with non-`'static` generic parameters can be mocked by
//! transforming the function arguments into trait objects.
#![deny(warnings)]

use mockall::*;
use std::path::{Path, PathBuf};

trait AsRefMut<T: ?Sized>: AsRef<T> + AsMut<T> {}
impl<Q, T> AsRefMut<T> for Q where Q: AsRef<T> + AsMut<T>, T: ?Sized {}

mock! {
    Foo {
        #[mockall::concretize]
        fn foo<P: AsRef<std::path::Path>>(&self, x: P);

        #[mockall::concretize]
        fn boom<P>(&self, x: P) where P: AsRef<std::path::Path>;

        #[mockall::concretize]
        fn bang<P: AsRef<std::path::Path>>(x: P);

        #[mockall::concretize]
        fn boomref<P: AsRef<std::path::Path>>(&self, x: &P);

        #[mockall::concretize]
        fn boom_mutref<T: AsRefMut<str>>(&self, x: &mut T);

        #[mockall::concretize]
        fn boomv<P>(&self, x: &[P]) where P: AsRef<std::path::Path>;
    }
}

mod generic_arg {
    use super::*;

    #[test]
    fn withf() {
        let mut foo = MockFoo::new();
        foo.expect_foo()
            .withf(|p| p.as_ref() == Path::new("/tmp"))
            .times(3)
            .return_const(());
        foo.foo(Path::new("/tmp"));
        foo.foo(PathBuf::from(Path::new("/tmp")));
        foo.foo("/tmp");
    }
}

mod where_clause {
    use super::*;

    #[test]
    fn withf() {
        let mut foo = MockFoo::new();
        foo.expect_boom()
            .withf(|p| p.as_ref() == Path::new("/tmp"))
            .times(3)
            .return_const(());
        foo.boom(Path::new("/tmp"));
        foo.boom(PathBuf::from(Path::new("/tmp")));
        foo.boom("/tmp");
    }
}

mod mutable_reference_arg {
    use super::*;

    #[test]
    fn withf() {
        let mut foo = MockFoo::new();
        foo.expect_boom_mutref()
            .withf(|p| p.as_ref() == "/tmp")
            .once()
            .returning(|s| s.as_mut().make_ascii_uppercase());
        let mut s = String::from("/tmp");
        foo.boom_mutref(&mut s);
        assert_eq!(s, "/TMP");
    }
}

mod reference_arg {
    use super::*;

    #[test]
    fn withf() {
        let mut foo = MockFoo::new();
        foo.expect_boomref()
            .withf(|p| p.as_ref() == Path::new("/tmp"))
            .times(3)
            .return_const(());
        foo.boomref(&Path::new("/tmp"));
        foo.boomref(&PathBuf::from(Path::new("/tmp")));
        foo.boomref(&"/tmp");
    }
}

mod slice {
    use super::*;

    #[test]
    fn withf() {
        let mut foo = MockFoo::new();
        foo.expect_boomv()
            .withf(|v|
                   v[0].as_ref() == Path::new("/tmp") &&
                   v[1].as_ref() == Path::new("/mnt")
            ).times(3)
            .return_const(());
        foo.boomv(&[Path::new("/tmp"), Path::new("/mnt")]);
        foo.boomv(&[PathBuf::from("/tmp"), PathBuf::from("/mnt")]);
        foo.boomv(&["/tmp", "/mnt"]);
    }
}

mod static_method {
    use super::*;

    #[test]
    fn withf() {
        let ctx = MockFoo::bang_context();
        ctx.expect()
            .withf(|p| p.as_ref() == Path::new("/tmp"))
            .times(3)
            .return_const(());
        MockFoo::bang(Path::new("/tmp"));
        MockFoo::bang(PathBuf::from(Path::new("/tmp")));
        MockFoo::bang("/tmp");
    }
}
