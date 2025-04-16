// vim: tw=80
//! Mock a trait like std::io::Read.  Mockall should not try to read the read
//! method's arguments if the test succeeds.
//! https://github.com/asomers/mockall/issues/647
#![deny(warnings)]

use std::{
    fmt::{self, Debug, Formatter},
    io
};

use mockall::*;

pub struct MyBuf<'a>(&'a mut [u8]);
impl<'a> Debug for MyBuf<'a> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        // An implementor of std::io::Read::read should not attempt to read from
        // the provided buffer, because it might be uninitialized.  Here, we
        // implement a panic in Debug::fmt, just to ensure that Mockall won't
        // try to format the mock method's arguments.
        panic!("Shouldn't attempt to read from me!");
    }
}

// Similarly, this wrapper type ensures that Mockall won't try to format such an
// object.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct NonDebuggable<T: 'static>(T);
impl<T: 'static> Debug for NonDebuggable<T> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        panic!("Shouldn't attempt to debug me!");
    }
}

trait Readlike {
    fn read<'a>(&mut self, buf: MyBuf<'a>) -> io::Result<usize>;
}

mock! {
    pub Foo {
        /* Also test some methods with different Expectation types */
        fn foo(&mut self, x: NonDebuggable<i32>) -> &usize;
        fn bar(&mut self, x: NonDebuggable<i32>) -> &mut usize;
        fn baz<T: 'static>(&mut self, x: NonDebuggable<T>) -> usize;
    }
    /* Just like std::io::Read, but with the MyBuf wrapper */
    impl Readlike for Foo {
        fn read<'a>(&mut self, buf: MyBuf<'a>) -> io::Result<usize>;
    }
}

const HELLO: &[u8] = b"Hello, World!\0";

#[test]
fn like_read() {
    let mut mock = MockFoo::new();
    let mut seq = Sequence::new();
    mock.expect_read()
        .times(1)
        .in_sequence(&mut seq)
        .returning(|buf| {
            buf.0[0..HELLO.len()].copy_from_slice(HELLO);
            Ok(HELLO.len())
        });

    let mut inner_buf = [0; 80];
    let my_buf = MyBuf(&mut inner_buf);
    mock.read(my_buf).unwrap();
    assert_eq!(&inner_buf[0..HELLO.len()], HELLO);
}

#[test]
fn return_ref() {
    let mut mock = MockFoo::new();
    let mut seq = Sequence::new();
    mock.expect_foo()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(42usize);

    assert_eq!(*mock.foo(NonDebuggable(5i32)), 42);
}

#[test]
fn return_refmut() {
    let mut mock = MockFoo::new();
    let mut seq = Sequence::new();
    mock.expect_bar()
        .times(1)
        .in_sequence(&mut seq)
        .return_var(42usize);

    assert_eq!(*mock.bar(NonDebuggable(5i32)), 42);
}

#[test]
fn generic() {
    let mut mock = MockFoo::new();
    let mut seq = Sequence::new();
    mock.expect_baz::<u32>()
        .times(1)
        .in_sequence(&mut seq)
        .return_const(42usize);

    assert_eq!(mock.baz(NonDebuggable(5u32)), 42);
}
