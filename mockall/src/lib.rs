// vim: tw=80

use downcast::*;
use std::{
    any,
    collections::hash_map::{DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    ops::DerefMut,
    sync::Mutex
};

trait ExpectationT : Any {}
downcast!(ExpectationT);

struct Expectation<I, O> {
    rfunc: Option<Box<FnMut(I) -> O>>
}

impl<I, O> Expectation<I, O> {
    fn new(rfunc: Option<Box<FnMut(I) -> O>>) -> Self {
        Expectation{rfunc}
    }
}

impl<I: 'static, O: 'static> ExpectationT for Expectation<I, O> {}

pub struct ExpectationBuilder<'object, I, O>
    where I: 'static, O: 'static
{
    e: &'object mut Expectations,
    rfunc: Option<Box<FnMut(I) -> O>>,
    ident: String
}

impl<'object, I, O> ExpectationBuilder<'object, I, O>
    where I: 'static, O: 'static
{
    fn new(e: &'object mut Expectations, ident: &str) -> Self {
        // Own the ident so we don't have to worry about lifetime issues
        let ident = ident.to_owned();
        ExpectationBuilder{rfunc: None, e, ident}
    }

    pub fn returning<F>(mut self, f: F) -> Self
        where F: FnMut(I) -> O + 'static
    {
        self.rfunc = Some(Box::new(f));
        self
    }
}

impl<'object, I, O> Drop for ExpectationBuilder<'object, I, O>
    where I: 'static, O: 'static
{
    fn drop(&mut self) {
        self.e.register(&self.ident, Expectation::new(self.rfunc.take()))
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

    pub fn called<I: 'static, O: 'static>(&self, ident: &str, args: I) -> O {
        let key = Key::new::<I, O>(ident);
        let mut guard = self.store.lock().unwrap();
        let store: &mut HashMap<_, _> = guard.deref_mut();
        let e: &mut Expectation<I, O> = store.get_mut(&key)
            .expect("No matching expectation found")
            .downcast_mut()
            .unwrap();
        e.rfunc.as_mut().unwrap()(args)
    }
}
