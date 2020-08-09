// vim: tw=80
//! Mock a struct that implements Serde traits
//!
//! Serde defines to popular traits that look like this:
//! ```ignore
//! trait Serialize {
//!     fn serialize<S: Serializer>(&self, serializer: S) -> Result<String, anyhow::Error>;
//! }
//! trait Deserialize {
//!     fn deserialize<D: Deserializer>(deserializer: D) -> Result<Self, anyhow::Error>;
//! }
//! ```
//!
//! Mockall can usually implement traits with generic methods.  However,
//! `serde::Serializer` isn't `'static`, and the definition of
//! `Serialize::serialize` doesn't require `S` to be static.  That's a problem,
//! because Mockall requires that all generic methods' generic types be
//! `'static` so that they can implement `std::any::Any`.
//!
//! There's no getting around that requirement.  But there's still hope!  You
//! can mock a struct that implements Serde traits by manually implementing
//! `Serialize` and `Deserialize` in terms of non-generic methods, using a
//! surrogate object for the expectations.
#![deny(warnings)]

use mockall::*;
use serde::{Deserialize, Deserializer};
use serde_derive::*;

/// A serializable surrogate for `Thing`.  It should serialize and deserialize
/// in exactly the same way as `Thing`.  In fact, it may even be `Thing`.
#[derive(Deserialize, Serialize)]
pub struct SurrogateThing {
    x: u32
}

mock! {
    pub Thing {
        // MockThing's private deserialize method.  The name is not important,
        // but you probably don't want to call it `deserialize` because then
        // you'll have to call the real deserialize method using
        // `<x as Deserialize>::deserialize` syntax.
        //
        // This method must always succeed (or panic), because `Deserialize`'s
        // error type is neither `'static` nor `Default`.
        fn private_deserialize(deserializable: Result<SurrogateThing, ()>) -> Self;
        // MockThing's private serialize method.  The name is not important,
        // but you probably don't want to call it `serialize` because then
        // you'll have to call the real serialize method using
        // `<x as Serialize>::serialize` syntax.
        //
        // This method must always succeed (or panic), because `Serialize`'s
        // error type is neither `'static` nor `Default`.
        fn private_serialize(&self) -> SurrogateThing;
    }
}

// Manually implement Serialize for MockThing
impl serde::Serialize for MockThing {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        self.private_serialize().serialize(s)
    }
}

// Manually implement Deserialize for MockThing
impl<'de> Deserialize<'de> for MockThing {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let serializable = SurrogateThing::deserialize(deserializer)
            .map_err(|_| ());
        Ok(MockThing::private_deserialize(serializable))
    }
}

// In your tests, set an expectation for `private_serialize` and return a
// suitable `SurrogateThing`
#[test]
fn serialize() {
    let mut mock = MockThing::default();
    mock.expect_private_serialize()
        .returning(|| SurrogateThing{x: 42} );

    let json = serde_json::to_string(&mock).unwrap();
    assert_eq!("{\"x\":42}", json);
}

// In your tests, set an expectation for `private_deserialize`, which will
// receive an already-deserialized `SurrogateThing` object.
#[test]
fn deserialize() {
    let ctx = MockThing::private_deserialize_context();
    ctx.expect()
        .withf(|st: &Result<SurrogateThing, ()>|
               st.as_ref().unwrap().x == 42
        ).once()
        .returning(|_| MockThing::default());

    let json = "{\"x\":42}";
    let _thing: MockThing = serde_json::from_str(json).unwrap();
}
