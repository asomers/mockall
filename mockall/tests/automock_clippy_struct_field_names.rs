#! vim: tw=80
//! One of the generated structs would trigger clippy::struct_field_names
//! https://github.com/asomers/mockall/issues/689
#![deny(clippy::struct_field_names)]

use mockall::*;

#[automock]
pub trait ExampleTrait {
    fn something_one(&self);
    fn something_two(&self);
    fn something_three(&self);
}

pub struct ExampleStruct {}

#[automock]
impl ExampleStruct {
    pub fn something_one(&self) {}
    pub fn something_two(&self) {}
    pub fn something_three(&self) {}
}
