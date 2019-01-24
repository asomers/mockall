// vim: tw=80
#![cfg_attr(feature = "nightly", feature(specialization))]

use cfg_if::cfg_if;
use downcast::*;
use std::{
    any,
    collections::hash_map::HashMap,
    mem,
    ops::{DerefMut, Range},
    sync::{
        Arc,
        Mutex,
        atomic::{AtomicUsize, Ordering}
    },
    thread
};

pub use mockall_derive::*;
pub use predicates::prelude::*;

trait AnyExpectations : Any + Send {}
downcast!(AnyExpectations);

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

struct Matcher<I>(Box<dyn Predicate<I> + Send>);

impl<I> Matcher<I> {
    fn matches(&self, i: &I) -> bool {
        self.0.eval(&i)
    }

    fn verify(&self, i: &I) {
        if let Some(case) = self.0.find_case(false, &i) {
            let c = &case as &predicates_tree::CaseTreeExt;
            panic!("Expectation didn't match arguments:\n{}", c.tree());
        }
    }
}

impl<I> Default for Matcher<I> {
    fn default() -> Self {
        Matcher(Box::new(predicate::always()))
    }
}

#[derive(Debug)]
struct Times{
    /// How many times has the expectation already been called?
    count: AtomicUsize,
    range: Range<usize>
}

impl Times {
    fn call(&self) {
        let count = self.count.fetch_add(1, Ordering::Relaxed) + 1;
        if count >= self.range.end {
            if self.range.end == 0 {
                panic!("Expectation should not have been called");
            } else {
                let lim = self.range.end - 1;
                panic!("Expectation called more than {} times", lim);
            }
        }
    }

    fn any(&mut self) {
        self.range = 0..usize::max_value();
    }

    /// Is it required that this expectation be called an exact number of times,
    /// or may it be satisfied by a range of call counts?
    fn is_exact(&self) -> bool {
        (self.range.end - self.range.start) == 1
    }

    fn n(&mut self, n: usize) {
        self.range = n..(n+1);
    }

    fn never(&mut self) {
        self.range = 0..0;
    }

    fn range(&mut self, range: Range<usize>) {
        self.range = range;
    }

    /// Has this expectation already been called the minimum required number of
    /// times?
    fn is_satisfied(&self) -> bool {
        self.count.load(Ordering::Relaxed) >= self.range.start
    }
}

impl Default for Times {
    fn default() -> Self {
        // By default, allow any number of calls
        let count = AtomicUsize::default();
        let range = 0..usize::max_value();
        Times{count, range}
    }
}

impl Drop for Times {
    fn drop(&mut self) {
        let count = self.count.load(Ordering::Relaxed);
        if !thread::panicking() && (count < self.range.start) {
            panic!("Expectation called fewer than {} times", self.range.start);
        }
    }
}

impl<I, O> Default for Expectation<I, O> {
    fn default() -> Self {
        Expectation::new()
    }
}

struct ExpectationCommon<I> {
    matcher: Matcher<I>,
    seq_handle: Option<SeqHandle>,
    times: Times
}

impl<I> ExpectationCommon<I> {
    pub fn call(&self, i: &I) {
        self.matcher.verify(i);
        self.times.call();
        self.verify_sequence();
        if self.times.is_satisfied() {
            self.satisfy_sequence()
        }
    }

    pub fn in_sequence(&mut self, seq: &mut Sequence) -> &mut Self {
        assert!(self.times.is_exact(),
            "Only Expectations with an exact call count have sequences");
        self.seq_handle = Some(seq.next());
        self
    }

    pub fn matches(&self, i: &I) -> bool {
        self.matcher.matches(i)
    }

    /// Forbid this expectation from ever being called
    pub fn never(&mut self) {
        self.times.never();
    }

    fn satisfy_sequence(&self) {
        if let Some(handle) = &self.seq_handle {
            handle.satisfy()
        }
    }

    /// Require this expectation to be called exactly `n` times.
    pub fn times(&mut self, n: usize) {
        self.times.n(n);
    }

    /// Allow this expectation to be called any number of times
    pub fn times_any(&mut self) {
        self.times.any();
    }

    /// Allow this expectation to be called any number of times within a given
    /// range
    pub fn times_range(&mut self, range: Range<usize>) {
        self.times.range(range);
    }

    fn verify_sequence(&self) {
        if let Some(handle) = &self.seq_handle {
            handle.verify()
        }
    }

    pub fn with<P>(&mut self, p: P)
        where P: Predicate<I> + Send + 'static
    {
        self.matcher = Matcher(Box::new(p));
    }
}

impl<I> Default for ExpectationCommon<I> {
    fn default() -> Self {
        ExpectationCommon {
            times: Times::default(),
            matcher: Matcher::default(),
            seq_handle: None,
        }
    }
}

/// Add common methods to Expectation types.  Must be called from within an
/// impl<I, O> {} block
macro_rules! expectation_common {
    ($common: ident, $klass:ident) => {
        pub fn in_sequence(&mut self, seq: &mut Sequence) -> &mut Self {
            self.$common.in_sequence(seq);
            self
        }

        fn matches(&self, i: &I) -> bool {
            self.common.matches(i)
        }

        /// Forbid this expectation from ever being called
        pub fn never(&mut self) -> &mut Self {
            self.$common.never();
            self
        }

        /// Require this expectation to be called exactly `n` times.
        pub fn times(&mut self, n: usize) -> &mut Self {
            self.$common.times(n);
            self
        }

        /// Allow this expectation to be called any number of times
        pub fn times_any(&mut self) -> &mut Self {
            self.$common.times_any();
            self
        }

        /// Allow this expectation to be called any number of times within a
        /// given range
        pub fn times_range(&mut self, range: Range<usize>) -> &mut Self {
            self.$common.times_range(range);
            self
        }

        pub fn with<P>(&mut self, p: P) -> &mut Self
            where P: Predicate<I> + Send + 'static
        {
            self.$common.with(p);
            self
        }
    }
}

/// Add common methods to Expectations types.  Must be called from within an
/// impl<I, O> {} block
macro_rules! expectations_common {
    ($klass:ident, $call:ident, $self_ty:ty, $iter:ident, $oty:ty) => {
        pub fn $call(self: $self_ty, i: I) -> $oty {
            match (self.0).$iter().rev().find(|e| e.matches(&i)) {
                None => panic!("No matching expectation found"),
                Some(e) => e.$call(i)
            }
        }

        pub fn expect(&mut self) -> &mut $klass<I, O> {
            let e = $klass::new();
            self.0.push(e);
            let l = self.0.len();
            &mut self.0[l - 1]
        }

        pub fn new() -> Self {
            Self::default()
        }
    }
}

/// Expectation type for methods that take return a `'static` type.
pub struct Expectation<I, O> {
    common: ExpectationCommon<I>,
    rfunc: Mutex<Rfunc<I, O>>,
}

impl<I, O> Expectation<I, O> {
    pub fn call(&self, i: I) -> O {
        self.common.call(&i);
        self.rfunc.lock().unwrap()
            .call_mut(i)
    }

    pub fn new() -> Self {
        let common = ExpectationCommon::default();
        let rfunc = Mutex::new(Rfunc::Default);
        Expectation{common, rfunc}
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

    expectation_common!{common, Expectation}
}

/// A Collection of `Expectation`s.  Allows you to set more than one expectation
/// on the same method.
pub struct Expectations<I, O>(Vec<Expectation<I, O>>);

impl<I, O> Expectations<I, O> {
    expectations_common!{Expectation, call, &Self, iter, O}
}

impl<I, O> Default for Expectations<I, O> {
    fn default() -> Self {
        Expectations(Vec::new())
    }
}

impl<I: 'static, O: 'static> AnyExpectations for Expectations<I, O> {}

/// Expectation type for methods that take a `&self` argument and return a
/// reference with the same lifetime as `self`.
pub struct RefExpectation<I, O> {
    common: ExpectationCommon<I>,
    result: Option<O>,
}

impl<I, O> RefExpectation<I, O> {
    pub fn call(&self, i: I) -> &O {
        self.common.call(&i);
        &self.result.as_ref()
            .expect("Must set return value with RefExpectation::return_const")
    }

    pub fn new() -> Self {
        let common = ExpectationCommon::default();
        RefExpectation{common, result: None}
    }

    pub fn return_const(&mut self, o: O) -> &mut Self {
        self.result = Some(o);
        self
    }

    expectation_common!{common, RefExpectation}
}

impl<I, O> Default for RefExpectation<I, O> {
    fn default() -> Self {
        RefExpectation::new()
    }
}

/// A Collection of `RefExpectation`s.  Allows you to set more than one
/// expectation on the same method.
pub struct RefExpectations<I, O>(Vec<RefExpectation<I, O>>);

impl<I, O> RefExpectations<I, O> {
    expectations_common!{RefExpectation, call, &Self, iter, &O}
}

impl<I, O> Default for RefExpectations<I, O> {
    fn default() -> Self {
        RefExpectations(Vec::new())
    }
}

impl<I, O> AnyExpectations for RefExpectations<I, O>
    where I: 'static, O: Send + 'static
{}

/// Expectation type for methods that take a `&mut self` argument and return a
/// mutable or immutable reference with the same lifetime as `self`.
pub struct RefMutExpectation<I, O> {
    common: ExpectationCommon<I>,
    result: Option<O>,
    rfunc: Option<Box<dyn FnMut(I) -> O + Send>>,
}

impl<I, O> RefMutExpectation<I, O> {
    pub fn call_mut(&mut self, i: I) -> &mut O {
        self.common.call(&i);
        if let Some(ref mut f) = self.rfunc {
            self.result = Some(f(i));
        }
        self.result.as_mut().expect("Must first set return function with RefMutExpectation::returning or return_var")
    }

    pub fn new() -> Self {
        let common = ExpectationCommon::default();
        RefMutExpectation{common, result: None, rfunc: None}
    }

    /// Convenience method that can be used to supply a return value for a
    /// `RefMutExpectation`.
    pub fn return_var(&mut self, o: O) -> &mut Self
    {
        self.result = Some(o);
        self
    }

    /// Supply a closure that the `RefMutExpectation` will use to create its
    /// return value.
    pub fn returning<F>(&mut self, f: F) -> &mut Self
        where F: FnMut(I) -> O + Send + 'static
    {
        mem::replace(&mut self.rfunc, Some(Box::new(f)));
        self
    }

    expectation_common!{common, RefMutExpectation}
}

impl<I, O> Default for RefMutExpectation<I, O> {
    fn default() -> Self {
        RefMutExpectation::new()
    }
}

/// A Collection of `RefMutExpectation`s.  Allows you to set more than one
/// expectation on the same method.
pub struct RefMutExpectations<I, O>(Vec<RefMutExpectation<I, O>>);

impl<I, O> RefMutExpectations<I, O> {
    expectations_common!{RefMutExpectation, call_mut, &mut Self, iter_mut,
        &mut O}
}

impl<I, O> Default for RefMutExpectations<I, O> {
    fn default() -> Self {
        RefMutExpectations(Vec::new())
    }
}

impl<I, O> AnyExpectations for RefMutExpectations<I, O>
    where I: 'static, O: Send + 'static
{}

/// Non-generic keys to `GenericExpectation` internal storage
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Key(any::TypeId);

impl Key {
    fn new<I: 'static, O: 'static>() -> Self {
        Key(any::TypeId::of::<(I, O)>())
    }
}

/// Expectation type for generic methods with `'static` return values.
/// Currently requires `'static` arguments as well.
#[derive(Default)]
pub struct GenericExpectations {
    store: HashMap<Key, Box<dyn AnyExpectations>>
}

impl GenericExpectations {
    pub fn call<I: 'static, O: 'static>(&self, args: I) -> O {
        let key = Key::new::<I, O>();
        let e: &Expectations<I, O> = self.store.get(&key)
            .expect("No matching expectation found")
            .downcast_ref()
            .unwrap();
        e.call(args)
    }

    pub fn expect<'e, I, O>(&'e mut self) -> &'e mut Expectation<I, O>
        where I: 'static, O: 'static
    {
        let key = Key::new::<I, O>();
        if ! self.store.contains_key(&key) {
            let expectations = Box::new(Expectations::<I, O>::new());
            self.store.insert(key.clone(), expectations);
        }
        let ee: &mut Expectations<I, O> = self.store.get_mut(&key).unwrap()
            .downcast_mut()
            .unwrap();
        ee.expect()
    }

    pub fn new() -> Self {
        Self::default()
    }
}

/// Expectation type for generic methods with a `&self` and return a reference
/// with the same lifetime as `self`.  Currently requires `'static` input
/// arguments as well.
#[derive(Default)]
pub struct GenericRefExpectations {
    store: HashMap<Key, Box<dyn AnyExpectations>>,
}

impl GenericRefExpectations {
    pub fn expect<'e, I, O>(&'e mut self) -> &'e mut RefExpectation<I, O>
        where I: 'static, O: Send + 'static
    {
        let key = Key::new::<I, O>();
        if ! self.store.contains_key(&key) {
            let expectations = Box::new(RefExpectations::<I, O>::new());
            self.store.insert(key.clone(), expectations);
        }
        let ee: &mut RefExpectations<I, O> = self.store.get_mut(&key).unwrap()
            .downcast_mut()
            .unwrap();
        ee.expect()
    }

    pub fn call<I: 'static, O: 'static>(&self, args: I) -> &O {
        let key = Key::new::<I, O>();
        let e: &RefExpectations<I, O> = self.store.get(&key)
            .expect("No matching expectation found")
            .downcast_ref()
            .unwrap();
        e.call(args)
    }
}

/// Expectation type for generic methods with a `&mut self` and return a
/// reference with the same lifetime as `self`.  Currently requires `'static`
/// input arguments as well.
#[derive(Default)]
pub struct GenericRefMutExpectations {
    store: HashMap<Key, Box<dyn AnyExpectations>>,
}

impl GenericRefMutExpectations {
    pub fn call_mut<I: 'static, O: 'static>(&mut self, args: I) -> &mut O {
        let key = Key::new::<I, O>();
        let e: &mut RefMutExpectations<I, O> = self.store.get_mut(&key)
            .expect("No matching expectation found")
            .downcast_mut()
            .unwrap();
        e.call_mut(args)
    }

    pub fn expect<'e, I, O>(&'e mut self) -> &'e mut RefMutExpectation<I, O>
        where I: 'static, O: Send + 'static
    {
        let key = Key::new::<I, O>();
        if ! self.store.contains_key(&key) {
            let expectations = Box::new(RefMutExpectations::<I, O>::new());
            self.store.insert(key.clone(), expectations);
        }
        let ee: &mut RefMutExpectations<I, O> = self.store.get_mut(&key)
            .unwrap()
            .downcast_mut()
            .unwrap();
        ee.expect()
    }
}

struct SeqHandle {
    inner: Arc<SeqInner>,
    seq: usize
}

impl SeqHandle {
    /// Tell the Sequence that this expectation has been fully satisfied
    fn satisfy(&self) {
        self.inner.satisfy(self.seq);
    }

    /// Verify that this handle was called in the correct order
    fn verify(&self) {
        self.inner.verify(self.seq);
    }
}

#[derive(Default)]
pub struct SeqInner {
    satisfaction_level: AtomicUsize,
}

impl SeqInner {
    /// Record the call identified by `seq` as fully satisfied.
    fn satisfy(&self, seq: usize) {
        let old_sl = self.satisfaction_level.fetch_add(1, Ordering::Relaxed);
        assert_eq!(old_sl, seq, "Method sequence violation.  Was an already-satisfied method called another time?");
    }

    /// Verify that the call identified by `seq` was called in the correct order
    fn verify(&self, seq: usize) {
        assert_eq!(seq, self.satisfaction_level.load(Ordering::Relaxed),
            "Method sequence violation")
    }
}

#[derive(Default)]
pub struct Sequence {
    inner: Arc<SeqInner>,
    next_seq: usize,
}

impl Sequence {
    pub fn new() -> Self {
        Self::default()
    }

    fn next(&mut self) -> SeqHandle {
        let handle = SeqHandle{inner: self.inner.clone(), seq: self.next_seq};
        self.next_seq += 1;
        handle
    }
}
