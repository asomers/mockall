//! A map and set that are safe for code generator use.
//!
//! *Do not add any IntoIterator impl to these.* Only insertion, membership
//! checking, and map lookup are exposed. Iteration order is not exposed.
//!
//! It's never correct for a code generator to produce nondeterministic output,
//! so iterating over a randomized hash based collection is bad. Even iterating
//! over a b-tree collection is non ideal -- it's always better to produce
//! output that follows source order. We enforce this in mockall_derive via
//! these wrapper types and the Clippy disallowed_type lint.

#![allow(clippy::disallowed_type)]

use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::iter::FromIterator;

#[derive(Debug)]
pub struct UnorderedMap<K, V>(HashMap<K, V>);

#[derive(Debug)]
pub struct UnorderedSet<T>(HashSet<T>);

impl<K, V> UnorderedMap<K, V> {
    pub fn new() -> Self {
        UnorderedMap(HashMap::new())
    }
}

impl<K, V> UnorderedMap<K, V>
where
    K: Eq + Hash,
{
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.0.insert(k, v)
    }

    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.0.contains_key(k)
    }

    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.0.get(k)
    }
}

impl<T> UnorderedSet<T>
where
    T: Eq + Hash,
{
    pub fn insert(&mut self, value: T) -> bool {
        self.0.insert(value)
    }

    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.0.contains(value)
    }

    pub fn extend(&mut self, set: Self) {
        self.0.extend(set.0)
    }
}

impl<K, V> Default for UnorderedMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Default for UnorderedSet<T> {
    fn default() -> Self {
        UnorderedSet(HashSet::default())
    }
}

impl<T> FromIterator<T> for UnorderedSet<T>
where
    T: Eq + Hash,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        UnorderedSet(HashSet::from_iter(iter))
    }
}
