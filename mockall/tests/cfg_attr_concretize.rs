// vim: tw=80
//! #[concretize] can be used inside of #[cfg_attr()]`
#![deny(warnings)]

use std::path::{Path, PathBuf};

use mockall::{automock, concretize};

#[automock]
trait Foo {
    #[cfg_attr(not(target_os = "ia64-unknown-multics"), concretize)]
    fn foo<P: AsRef<Path>>(&self, p: P);
}


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
