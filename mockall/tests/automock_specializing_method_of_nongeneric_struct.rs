// vim: tw=80
//! Non-generic structs can have specializing methods, too.  For example, some
//! methods place constraints on Self.  It's even possible for a where clause to
//! place a constraint on types that appear nowhere in the struct's signatures.
#![deny(warnings)]

use mockall::*;

#[automock]
trait Bar {
    fn bar(&self) where Self: Sized;
    fn baz() where Self: Sized;
}

#[test]
fn nonstatic() {
    let mut mock = MockBar::default();
    mock.expect_bar()
        .return_const(());
    mock.bar();
}

#[test]
fn static_method() {
    let ctx = MockBar::baz_context();
    ctx.expect()
        .return_const(());
    MockBar::baz();
}
