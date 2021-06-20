// vim: tw=80
//! A mocked struct should implement Debug
#![deny(warnings)]

use mockall::*;
use std::fmt::{self, Debug, Formatter};

// using derive(Debug) tells mockall to generate the Debug impl automatically
mock!{
    #[derive(Debug)]
    pub Bar {
        fn foo(&self) -> u32;
    }
    impl Clone for Bar {
        fn clone(&self) -> Self;
    }
}

// With no derive(Debug), mockall won't genetate the debug impl automatically
mock!{
    pub Baz {
        fn foo(&self) -> u32;
    }
    impl Clone for Baz {
        fn clone(&self) -> Self;
    }
}
impl Debug for MockBaz {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        f.debug_struct("XXX").finish()
    }
}

#[test]
fn automatic() {
    let bar = MockBar::new();
    assert_eq!("MockBar", format!("{:?}", bar));
}

#[test]
fn manual() {
    let baz = MockBaz::new();
    assert_eq!("XXX", format!("{:?}", baz));
}
