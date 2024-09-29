// vim: tw=80
//! Using automock on a struct with private methods should not result in any
//! "dead_code" warnings.  Such methods are often private helpers used only by
//! other public methods.
#![deny(dead_code)]

pub mod mymod {
    use mockall::automock;

    pub struct Foo{}

    #[automock]
    impl Foo {
        fn private_helper(&self) {}
        fn static_private_helper() {}

        pub fn pubfunc(&self) {
            Self::static_private_helper();
            self.private_helper()
        }
    }
}
