// vim: tw=80
//! A generic mock object that implements Future
//!
//! This is tricky because the Context object has a lifetime parameter, yet the
//! poll method must not be treated as a generic method.
#![deny(warnings)]

use futures::executor::block_on;
use mockall::*;
use std::{
    future::Future,
    pin::Pin,
    task::{Context, Poll},
};

struct Foo<T>(T);

#[automock]
impl<T> Future for Foo<T> {
    type Output = ();

    fn poll<'a>(self: Pin<&mut Self>, _cx: &mut Context<'a>)
        -> Poll<Self::Output>
    {
        unimplemented!()
    }
}

#[test]
fn ready() {
    let mut mock = MockFoo::<u32>::new();
    mock.expect_poll()
        .once()
        .return_const(Poll::Ready(()));
    block_on(mock);
}
