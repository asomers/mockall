// vim: tw=80
//! A native async trait with trait_variant, for use with Futures
#![deny(warnings)]

use futures::executor::block_on;
use mockall::*;

mod simple {
    use super::*;

    #[trait_variant::make(Send)]
    pub trait Foo {
        async fn foo(&self) -> u32;
    }

    mock! {
        pub Bar { }
        #[trait_variant::make(Send)]
        impl Foo for Bar {
            async fn foo(&self) -> u32;
        }
    }

    #[test]
    fn return_const() {
        let mut mock = MockBar::new();
        mock.expect_foo()
            .return_const(42u32);
        assert_eq!(block_on(mock.foo()), 42);
    }
}

/// Mock the original variant of the trait (`LocalFoo`), even if there's another Sendable version (`Foo`)
mod original {
    use super::*;

    #[trait_variant::make(Foo: Send)]
    pub trait LocalFoo {
        async fn foo(&self) -> u32;
    }

    mock! {
        pub LocalBar { }
        #[trait_variant::make(Send)]
        impl LocalFoo for LocalBar {
            async fn foo(&self) -> u32;
        }
    }

    #[test]
    fn return_const() {
        let mut mock = MockLocalBar::new();
        mock.expect_foo()
            .return_const(42u32);
        assert_eq!(block_on(mock.foo()), 42);
    }
}

/// Mock the Sendable version of the trait (`Foo`)
/// It will also implement `LocalFoo` because `trait-variant` adds a blanket impl
mod variant {
    use super::*;

    #[trait_variant::make(Foo: Send)]
    pub trait LocalFoo {
        async fn foo(&self) -> u32;
    }

    mock! {
        pub Bar { }
        #[trait_variant::make(Send)]
        impl Foo for Bar {
            async fn foo(&self) -> u32;
        }
    }

    #[test]
    fn return_const() {
        let mut mock = MockBar::new();
        mock.expect_foo()
            .return_const(42u32);
        assert_eq!(block_on(Foo::foo(&mock)), 42);
        assert_eq!(block_on(LocalFoo::foo(&mock)), 42);
    }
}