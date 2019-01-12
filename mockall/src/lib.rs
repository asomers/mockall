// vim: tw=80
#![cfg_attr(feature = "nightly", feature(specialization))]

use cfg_if::cfg_if;
use downcast::*;
use std::{
    any,
    collections::hash_map::{DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    mem,
    ops::DerefMut,
    sync::Mutex
};

trait ExpectationT : Any {}
downcast!(ExpectationT);

/// Return functions for expectations
enum Rfunc<I, O> {
    Default,
    Mut(Box<dyn FnMut(I) -> O>)
}

// TODO: change this to "impl FnMut" once unboxed_closures are stable
// https://github.com/rust-lang/rust/issues/29625
impl<I, O>  Rfunc<I, O> {
    fn call_mut(&mut self, args: I) -> O {
        match self {
            Rfunc::Default => {
                Self::return_default()
            },
            Rfunc::Mut(f) => {
                f(args)
            }
        }
    }
}

trait ReturnDefault<O> {
    fn return_default() -> O;
}

cfg_if! {
    if #[cfg(feature = "nightly")] {
        impl<I, O> ReturnDefault<O> for Rfunc<I, O> {
            default fn return_default() -> O {
                panic!("Can only return default values for types that impl std::Default");
            }
        }
    } else {
        impl<I, O> ReturnDefault<O> for Rfunc<I, O> {
            fn return_default() -> O {
                panic!("Returning default values requires the \"nightly\" feature");
            }
        }
    }
}

#[cfg(feature = "nightly")]
impl<I, O: Default> ReturnDefault<O> for Rfunc<I, O> {
    fn return_default() -> O {
        O::default()
    }
}

struct Expectation<I, O> {
    rfunc: Rfunc<I, O>
}

impl<I, O> Expectation<I, O> {
    fn new(rfunc: Rfunc<I, O>) -> Self {
        Expectation{rfunc}
    }
}

impl<I: 'static, O: 'static> ExpectationT for Expectation<I, O> {}

pub struct ExpectationBuilder<'object, I, O>
    where I: 'static, O: 'static
{
    e: &'object mut Expectations,
    rfunc: Rfunc<I, O>,
    ident: String
}

impl<'object, I, O> ExpectationBuilder<'object, I, O>
    where I: 'static, O: 'static
{
    fn new(e: &'object mut Expectations, ident: &str) -> Self {
        // Own the ident so we don't have to worry about lifetime issues
        let ident = ident.to_owned();
        ExpectationBuilder{rfunc: Rfunc::Default, e, ident}
    }

    pub fn returning<F>(mut self, f: F) -> Self
        where F: FnMut(I) -> O + 'static
    {
        self.rfunc = Rfunc::Mut(Box::new(f));
        self
    }
}

impl<'object, I, O> Drop for ExpectationBuilder<'object, I, O>
    where I: 'static, O: 'static
{
    fn drop(&mut self) {
        let rfunc = mem::replace(&mut self.rfunc, Rfunc::Default);
        self.e.register(&self.ident, Expectation::new(rfunc))
    }
}

/// Non-generic keys to `Expectations` internal storage
#[derive(Debug, Eq, Hash, PartialEq)]
struct Key(u64);

impl Key {
    fn new<I: 'static, O: 'static>(ident: &str) -> Self {
        let mut hasher = DefaultHasher::new();
        ident.hash(&mut hasher);
        any::TypeId::of::<(I, O)>().hash(&mut hasher);
        Key(hasher.finish())
    }
}

#[derive(Default)]
pub struct Expectations {
    store: Mutex<HashMap<Key, Box<dyn ExpectationT>>>
}

impl Expectations {
    fn register<I, O>(&mut self, ident: &str, expectation: Expectation<I, O>)
        where I: 'static, O: 'static
    {
        let key = Key::new::<I, O>(ident);
        let store = self.store.get_mut().unwrap();
        store.insert(key, Box::new(expectation));
    }

    pub fn expect<I, O>(&mut self, ident: &str) -> ExpectationBuilder<I, O>
        where I: 'static, O: 'static
    {
        ExpectationBuilder::<I, O>::new(self, ident)
    }

    // TODO: add a "called_nonstatic" method that can be used with types that
    // aren't 'static, and uses a different method to generate the key.
    pub fn called<I: 'static, O: 'static>(&self, ident: &str, args: I) -> O {
        let key = Key::new::<I, O>(ident);
        let mut guard = self.store.lock().unwrap();
        let store: &mut HashMap<_, _> = guard.deref_mut();
        let e: &mut Expectation<I, O> = store.get_mut(&key)
            .expect("No matching expectation found")
            .downcast_mut()
            .unwrap();
        e.rfunc.call_mut(args)
    }
}
