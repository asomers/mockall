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

trait ExpectationT : Any + Send {}
downcast!(ExpectationT);

trait ReturnDefault<O> {
    fn return_default() -> O;
}

/// Return functions for expectations
enum Rfunc<I, O> {
    Default,
    // Indicates that a `return_once` expectation has already returned
    Expired,
    Mut(Box<dyn FnMut(I) -> O + Send>),
    // Should be Box<dyn FnOnce> once that feature is stabilized
    // https://github.com/rust-lang/rust/issues/28796
    Once(Box<dyn FnMut(I) -> O + Send>),
}

// TODO: change this to "impl FnMut" once unboxed_closures are stable
// https://github.com/rust-lang/rust/issues/29625
impl<I, O>  Rfunc<I, O> {
    fn call_mut(&mut self, args: I) -> O {
        match self {
            Rfunc::Default => {
                Self::return_default()
            },
            Rfunc::Expired => {
                panic!("Called a method twice that was expected only once")
            },
            Rfunc::Mut(f) => {
                f(args)
            },
            Rfunc::Once(_) => {
                let fo = mem::replace(self, Rfunc::Expired);
                if let Rfunc::Once(mut f) = fo {
                    f(args)
                } else {
                    unreachable!()
                }
            },
        }
    }
}

impl<I, O> Default for Rfunc<I, O> {
    fn default() -> Self {
        Rfunc::Default
    }
}

cfg_if! {
    if #[cfg(feature = "nightly")] {
        impl<I, O> ReturnDefault<O> for Rfunc<I, O> {
            default fn return_default() -> O {
                panic!("Can only return default values for types that impl std::Default");
            }
        }

        impl<I, O: Default> ReturnDefault<O> for Rfunc<I, O> {
            fn return_default() -> O {
                O::default()
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

pub struct Expectation<I, O> {
    rfunc: Mutex<Rfunc<I, O>>
}

impl<I, O> Default for Expectation<I, O> {
    fn default() -> Self {
        Expectation::new()
    }
}

impl<I, O> Expectation<I, O> {
    pub fn call(&self, i: I) -> O {
        self.rfunc.lock().unwrap()
            .call_mut(i)
    }

    pub fn new() -> Self {
        let rfunc = Mutex::new(Rfunc::Default);
        Expectation{rfunc}
    }

    pub fn returning<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + Send + 'static
    {
        {
            let mut guard = self.rfunc.lock().unwrap();
            mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
        }
        self
    }

    pub fn return_once<F>(&mut self, f: F) -> &mut Self
        where F: FnOnce(I) -> O + Send + 'static
    {
        let mut fopt = Some(f);
        let fmut = move |i| {
            if let Some(f) = fopt.take() {
                f(i)
            } else {
                panic!("Called a method twice that was expected only once")
            }
        };
        {
            let mut guard = self.rfunc.lock().unwrap();
            mem::replace(guard.deref_mut(), Rfunc::Once(Box::new(fmut)));
        }
        self
    }
}

impl<I: 'static, O: 'static> ExpectationT for Expectation<I, O> {}

/// Expectation type for methods that take a `&self` argument and return a
/// reference with the same lifetime as `self`.
#[derive(Default)]
pub struct RefExpectation<I, O> {
    result: Option<O>,
    phantom: std::marker::PhantomData<I> ,
}

impl<I, O> RefExpectation<I, O> {
    pub fn call(&self, _i: I) -> &O {
        &self.result.as_ref()
            .expect("Must set return value with RefExpectation::return_const")
    }

    pub fn return_const(&mut self, o: O) -> &mut Self {
        self.result = Some(o);
        self
    }
}

impl<I, O> ExpectationT for RefExpectation<I, O>
    where I: Send + 'static, O: Send + 'static
{}

/// Expectation type for methods that take a `&mut self` argument and return a
/// mutable or immutable reference with the same lifetime as `self`.
#[derive(Default)]
pub struct RefMutExpectation<I, O> {
    result: Option<O>,
    rfunc: Option<Box<dyn FnMut(I) -> O + Send>>
}

impl<I, O> RefMutExpectation<I, O> {
    pub fn call_mut(&mut self, i: I) -> &mut O {
        let r = self.rfunc
            .as_mut()
            .expect("Must first set return function with RefMutExpectation::returning")
            (i);
        self.result = Some(r);
        self.result.as_mut().unwrap()
    }

    /// Supply a closure that the `RefMutExpectation` will use to create its
    /// return value.
    pub fn returning<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + Send + 'static
    {
        mem::replace(&mut self.rfunc, Some(Box::new(f)));
        self
    }
}

impl<I, O> ExpectationT for RefMutExpectation<I, O>
    where I: 'static, O: Send + 'static
{}

/// Non-generic keys to `GenericExpectations` internal storage
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
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
pub struct GenericExpectations {
    store: HashMap<Key, Box<dyn ExpectationT>>
}

impl GenericExpectations {
    pub fn expect<'e, I, O>(&'e mut self, ident: &str)
        -> &'e mut Expectation<I, O>
        where I: 'static, O: 'static
    {
        let key = Key::new::<I, O>(ident);
        let e = Box::new(Expectation::<I, O>::new());
        self.store.insert(key.clone(), e);
        self.store.get_mut(&key).unwrap()
            .downcast_mut()
            .unwrap()
    }

    // TODO: add a "called_nonstatic" method that can be used with types that
    // aren't 'static, and uses a different method to generate the key.
    pub fn called<I: 'static, O: 'static>(&self, ident: &str, args: I) -> O {
        let key = Key::new::<I, O>(ident);
        let e: &Expectation<I, O> = self.store.get(&key)
            .expect("No matching expectation found")
            .downcast_ref()
            .unwrap();
        e.call(args)
    }
}
