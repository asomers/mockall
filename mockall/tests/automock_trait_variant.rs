// vim: tw=80
//! A native async trait with trait_variant, for use with Futures
#![deny(warnings)]

use futures::executor::block_on;
use mockall::*;

mod simple {
    use super::*;

    #[automock]
    #[trait_variant::make(Send)]
    pub trait Foo {
        async fn foo(&self) -> u32;
        async fn bar() -> u32;
    }
    
    #[test]
    fn return_const() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .return_const(42u32);
        assert_eq!(block_on(mock.foo()), 42);
    }
    
    #[test]
    fn static_method() {
        let ctx = MockFoo::bar_context();
        ctx.expect()
            .return_const(42u32);
        assert_eq!(block_on(MockFoo::bar()), 42);
    }
}

/// Mock the original variant of the trait (`LocalFoo`), even if there's another Sendable version (`Foo`)
mod original {
    use super::*;

    #[automock]
    #[trait_variant::make(Foo: Send)]
    pub trait LocalFoo {
        async fn foo(&self) -> u32;
        async fn bar() -> u32;
    }
    
    #[test]
    fn return_const() {
        let mut mock = MockLocalFoo::new();
        mock.expect_foo()
            .return_const(42u32);
        assert_eq!(block_on(mock.foo()), 42);
    }
    
    #[test]
    fn static_method() {
        let ctx = MockLocalFoo::bar_context();
        ctx.expect()
            .return_const(42u32);
        assert_eq!(block_on(MockLocalFoo::bar()), 42);
    }
}

/// Mock the Sendable version of the trait (`Foo`)
/// It will also implement `LocalFoo` because `trait-variant` adds a blanket impl
mod variant {
    use super::*;

    #[automock(target = Foo)]
    #[trait_variant::make(Foo: Send)]
    pub trait LocalFoo {
        async fn foo(&self) -> u32;
        async fn bar() -> u32;
    }
    
    #[test]
    fn return_const() {
        let mut mock = MockFoo::new();
        mock.expect_foo()
            .return_const(42u32);
        assert_eq!(block_on(Foo::foo(&mock)), 42);
        assert_eq!(block_on(LocalFoo::foo(&mock)), 42);
    }
    
    #[test]
    fn static_method() {
        let ctx = MockFoo::bar_context();
        ctx.expect()
            .return_const(42u32);
        assert_eq!(block_on(<MockFoo as Foo>::bar()), 42);
        assert_eq!(block_on(<MockFoo as LocalFoo>::bar()), 42);
    }
}
