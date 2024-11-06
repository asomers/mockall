//! Mockall should suppress clippy::ref_option warnings in generated code.
#![warn(clippy::ref_option)]

#[mockall::automock]
pub trait Store {
    fn find(&self, name: Option<String>) -> bool;
}
