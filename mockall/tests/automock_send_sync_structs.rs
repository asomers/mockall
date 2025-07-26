// vim: tw=80
//! automocking when a custom `Send` and `Sync` struct are in scope
// This example only shows that things compile properly
#![allow(dead_code)]

use mockall::*;

struct Send;

struct Sync;

#[automock]
trait A {
    fn bar() -> u32;
}
