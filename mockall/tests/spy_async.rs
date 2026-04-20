// vim: tw=80
//! Async method spy tests: spy! and #[autospy] with async_trait
#![deny(warnings)]

use async_trait::async_trait;
use futures::executor::block_on;
use mockall::*;

// ── spy! with async_trait ───────────────────────────────────────────

#[async_trait]
trait AsyncCalc: Send + Sync {
    async fn compute(&self, x: u32) -> u32;
    async fn greet(&self, name: &str) -> String;
}

struct RealCalc;

#[async_trait]
impl AsyncCalc for RealCalc {
    async fn compute(&self, x: u32) -> u32 {
        x * 3
    }
    async fn greet(&self, name: &str) -> String {
        format!("hello {name}")
    }
}

spy! {
    Calc {}
    #[async_trait]
    impl AsyncCalc for Calc {
        async fn compute(&self, x: u32) -> u32;
        async fn greet(&self, name: &str) -> String;
    }
}

mod spy_macro {
    use super::*;

    #[test]
    fn delegates_async_method() {
        let spy = SpyCalc::new(RealCalc);
        assert_eq!(block_on(spy.compute(7)), 21);
    }

    #[test]
    fn delegates_async_method_returning_string() {
        let spy = SpyCalc::new(RealCalc);
        assert_eq!(block_on(spy.greet("world")), "hello world");
    }

    #[test]
    fn override_async_method() {
        let mut spy = SpyCalc::new(RealCalc);
        spy.expect_compute()
            .returning(|x| x + 100);
        assert_eq!(block_on(spy.compute(5)), 105);
    }

    #[test]
    fn calling_real_async() {
        let mut spy = SpyCalc::new(RealCalc);
        spy.expect_compute()
            .times(1)
            .calling_real();
        assert_eq!(block_on(spy.compute(10)), 30);
    }

    #[test]
    fn partial_mock_async() {
        let mut spy = SpyCalc::new(RealCalc);
        spy.expect_compute()
            .returning(|_| 999);
        // overridden
        assert_eq!(block_on(spy.compute(1)), 999);
        // non-overridden still delegates
        assert_eq!(block_on(spy.greet("rust")), "hello rust");
    }
}

// ── #[autospy] with async_trait ─────────────────────────────────────

#[autospy]
#[async_trait]
trait AsyncStore: Send + Sync {
    async fn get(&self, key: &str) -> Option<String>;
    async fn put(&mut self, key: &str, value: &str) -> bool;
}

struct MemStore;

#[async_trait]
impl AsyncStore for MemStore {
    async fn get(&self, _key: &str) -> Option<String> {
        Some("stored".to_string())
    }
    async fn put(&mut self, _key: &str, _value: &str) -> bool {
        true
    }
}

mod autospy_attr {
    use super::*;

    #[test]
    fn delegates_async_get() {
        let spy = SpyAsyncStore::new(MemStore);
        assert_eq!(
            block_on(spy.get("k")),
            Some("stored".to_string())
        );
    }

    #[test]
    fn delegates_async_put() {
        let mut spy = SpyAsyncStore::new(MemStore);
        assert!(block_on(spy.put("k", "v")));
    }

    #[test]
    fn override_async_get() {
        let mut spy = SpyAsyncStore::new(MemStore);
        spy.expect_get()
            .returning(|_| None);
        assert_eq!(block_on(spy.get("k")), None);
    }

    #[test]
    fn calling_real_async() {
        let mut spy = SpyAsyncStore::new(MemStore);
        spy.expect_get()
            .times(1)
            .calling_real();
        assert_eq!(
            block_on(spy.get("k")),
            Some("stored".to_string())
        );
    }
}
