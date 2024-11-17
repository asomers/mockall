//! It should be possible to use #[expect::clippy] within an #[automock] block

use mockall::automock;

pub struct Foo {}

#[automock]
impl Foo {
    #[expect(unused_variables)]
    pub fn foo(&self) {
        let a = 0;
    }
}
