// vim: tw=80
//! A trait with a constructor method that returns impl Future<...>.
//!
//! This needs special handling, because Box<dyn Future<...>> is pretty useless.
//! You need Pin<Box<dyn Future<...>>> instead.
#![deny(warnings)]

use futures::{Future, FutureExt, Stream, StreamExt, future, stream};
use mockall::*;

pub struct Foo{}

#[automock]
impl Foo {
    pub fn foo(&self) -> impl Future<Output=u32>
    {
        future::ready(42)
    }

    pub fn bar(&self) -> impl Stream<Item=u32>
    {
        stream::empty()
    }
}

#[test]
fn returning_future() {
    let mut mock = MockFoo::new();
    mock.expect_foo()
        .returning(|| {
            Box::pin(future::ready(42))
        });
    mock.foo()
        .now_or_never()
        .unwrap();
}

#[test]
fn returning_stream() {
    let mut mock = MockFoo::new();
    mock.expect_bar()
        .returning(|| {
            Box::pin(stream::iter(vec![42].into_iter()))
        });
    let all = mock.bar()
        .collect::<Vec<u32>>()
        .now_or_never()
        .unwrap();
    assert_eq!(&all[..], &[42][..]);
}
