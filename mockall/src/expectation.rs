// vim: tw=80

/// The Matcher class is common to all Expectation types
#[macro_export]
#[doc(hidden)]
macro_rules! common_matcher {
    ([$($generics:ident, )*] [$($args:ident)*]
     [$($altargs:ident)*] [$($matchty:ty)*]) =>
    {
        enum Matcher<$($generics: 'static,)*> {
            Func(Box<Fn($( &$matchty, )* ) -> bool + Send>),
            Pred( $( Box<$crate::Predicate<$matchty> + Send>, )* ),
            // Prevent "unused type parameter" errors
            // Surprisingly, PhantomData<Fn(generics)> is Send even if generics
            // are not, unlike PhantomData<generics>
            _Phantom(Box<Fn($( $generics, )*) -> () + Send>)
        }

        impl<$($generics: 'static,)*> Matcher<$($generics,)*> {
            fn matches(&self, $( $args: &$matchty, )*) -> bool {
                match self {
                    Matcher::Func(f) => f($( $args, )*),
                    Matcher::Pred($( $altargs, )*) =>
                        [$( $altargs.eval($args), )*]
                        .into_iter()
                        .all(|x| *x),
                    _ => unreachable!()
                }
            }

            fn verify(&self, $( $args: &$matchty, )* ) {
                match self {
                    Matcher::Func(f) => assert!(f($( $args, )*),
                        "Expectation didn't match arguments"),
                    Matcher::Pred($( $altargs, )*) => {
                        $(if let Some(c) = $altargs.find_case(false, $args) {
                            panic!("Expectation didn't match arguments:\n{}",
                                   c.tree());
                        })*
                    },
                    _ => unreachable!()
                }
            }
        }

        impl<$($generics: 'static,)*> Default for Matcher<$($generics,)*> {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|$( $args, )*| true))
            }
        }

    }
}

/// Common methods of all Expectation types
#[macro_export]
#[doc(hidden)]
macro_rules! common_methods {
    ([$($generics:ident, )*] [$($args:ident)*]
     [$($altargs:ident)*] [$($matchty:ty)*]) =>
    {
        /// Holds the stuff that is independent of the output type
        struct Common<$($generics: 'static,)*> {
            matcher: Mutex<Matcher<$($generics,)*>>,
            seq_handle: Option<$crate::SeqHandle>,
            times: $crate::Times
        }

        impl<$($generics: 'static,)*> std::default::Default for Common<$($generics,)*>
        {
            fn default() -> Self {
                Common {
                    matcher: Mutex::new(Matcher::default()),
                    seq_handle: None,
                    times: $crate::Times::default()
                }
            }
        }

        impl<$($generics: 'static,)*> Common<$($generics,)*> {
            fn call(&self, $( $args: &$matchty, )* ) {
                self.matcher.lock().unwrap().verify($( $args, )*);
                self.times.call();
                self.verify_sequence();
                if self.times.is_satisfied() {
                    self.satisfy_sequence()
                }
            }

            fn in_sequence(&mut self, seq: &mut $crate::Sequence)
                -> &mut Self
            {
                assert!(self.times.is_exact(),
                    "Only Expectations with an exact call count have sequences");
                self.seq_handle = Some(seq.next());
                self
            }

            fn is_done(&self) -> bool {
                self.times.is_done()
            }

            fn matches(&self, $( $args: &$matchty, )*) -> bool {
                self.matcher.lock().unwrap().matches($( $args, )*)
            }

            /// Forbid this expectation from ever being called.
            fn never(&mut self) {
                self.times.never();
            }

            fn satisfy_sequence(&self) {
                if let Some(handle) = &self.seq_handle {
                    handle.satisfy()
                }
            }

            /// Expect this expectation to be called exactly `n` times.
            fn times(&mut self, n: usize) {
                self.times.n(n);
            }

            /// Allow this expectation to be called any number of times
            fn times_any(&mut self) {
                self.times.any();
            }

            /// Allow this expectation to be called any number of times within a
            /// given range
            fn times_range(&mut self, range: Range<usize>) {
                self.times.range(range);
            }

            #[allow(non_camel_case_types)]  // Repurpose $altargs for generics
            fn with<$( $altargs: $crate::Predicate<$matchty> + Send + 'static,)*>
                (&mut self, $( $args: $altargs,)*)
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred($( Box::new($args), )*);
                mem::replace(guard.deref_mut(), m);
            }

            fn withf<F>(&mut self, f: F)
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Func(Box::new(f));
                mem::replace(guard.deref_mut(), m);
            }

            fn verify_sequence(&self) {
                if let Some(handle) = &self.seq_handle {
                    handle.verify()
                }
            }
        }
    }
}

/// Common methods of the Expectation structs
#[macro_export]
#[doc(hidden)]
macro_rules! expectation_methods {
    ($v:vis [$($args:ident)*] [$($altargs:ident)*] [$($matchty:ty)*]) => {
        /// Add this expectation to a [`Sequence`](../../../mockall/struct.Sequence.html).
        $v fn in_sequence(&mut self, seq: &mut $crate::Sequence) -> &mut Self {
            self.common.in_sequence(seq);
            self
        }

        fn is_done(&self) -> bool {
            self.common.is_done()
        }

        /// Validate this expectation's matcher.
        fn matches(&self, $( $args: &$matchty, )*) -> bool {
            self.common.matches($( $args, )*)
        }

        /// Forbid this expectation from ever being called.
        $v fn never(&mut self) -> &mut Self {
            self.common.never();
            self
        }

        /// Create a new, default, [`Expectation`](struct.Expectation.html)
        $v fn new() -> Self {
            Self::default()
        }

        /// Expect this expectation to be called exactly once.  Shortcut for
        /// [`times(1)`](#method.times).
        $v fn once(&mut self) -> &mut Self {
            self.times(1)
        }

        /// Expect this expectation to be called exactly `n` times.
        $v fn times(&mut self, n: usize) -> &mut Self {
            self.common.times(n);
            self
        }

        /// Allow this expectation to be called any number of times
        ///
        /// This behavior is the default, but the method is provided in case the
        /// default behavior changes.
        $v fn times_any(&mut self) -> &mut Self {
            self.common.times_any();
            self
        }

        /// Allow this expectation to be called any number of times within a
        /// given range
        $v fn times_range(&mut self, range: Range<usize>)
            -> &mut Self
        {
            self.common.times_range(range);
            self
        }

        /// Set matching crieteria for this Expectation.
        ///
        /// The matching predicate can be anything implemening the
        /// [`Predicate`](../../../mockall/trait.Predicate.html) trait.  Only
        /// one matcher can be set per `Expectation` at a time.
        #[allow(non_camel_case_types)]  // Repurpose $altargs for generics
        $v fn with<$( $altargs: $crate::Predicate<$matchty> + Send + 'static,)*>
            (&mut self, $( $args: $altargs,)*) -> &mut Self
        {
            self.common.with($( $args, )*);
            self
        }

        /// Set a matching function for this Expectation.
        ///
        /// This is equivalent to calling [`with`](#method.with) with a function
        /// argument, like `with(predicate::function(f))`.
        $v fn withf<F>(&mut self, f: F) -> &mut Self
            where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
        {
            self.common.withf(f);
            self
        }

    }
}

/// Common methods of the Expectations structs
#[macro_export]
#[doc(hidden)]
macro_rules! expectations_methods {
    ($v:vis [$($generics:ident, )*]) => {
        /// A collection of [`Expectation`](struct.Expectations.html) objects.
        /// Users will rarely if ever use this struct directly.
        #[doc(hidden)]
        $v struct Expectations<$($generics: 'static,)*>(
            Vec<Expectation<$($generics,)*>>
        );

        impl<$($generics: 'static,)*> Expectations<$($generics,)*> {
            /// Verify that all current expectations are satisfied and clear
            /// them.
            $v fn checkpoint(&mut self) {
                self.0.drain(..);
            }

            /// Create a new expectation for this method.
            $v fn expect(&mut self) -> &mut Expectation<$($generics,)*>
            {
                let e = Expectation::default();
                self.0.push(e);
                let l = self.0.len();
                &mut self.0[l - 1]
            }

            $v fn new() -> Self {
                Self::default()
            }
        }
        impl<$($generics: 'static,)*> Default for Expectations<$($generics,)*>
        {
            fn default() -> Self {
                Expectations(Vec::new())
            }
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! generic_expectation_methods {
    ($v:vis [$($generics:ident, )*] [$($argty:ty)*] $o:ty) =>
    {
        /// A collection of [`Expectation`](struct.Expectations.html) objects
        /// for a generic method.  Users will rarely if ever use this struct
        /// directly.
        #[doc(hidden)]
        #[derive(Default)]
        $v struct GenericExpectations{
            store: HashMap<$crate::Key, Box<dyn $crate::AnyExpectations>>
        }
        impl GenericExpectations {
            /// Verify that all current expectations are satisfied and clear
            /// them.  This applies to all sets of generic parameters!
            $v fn checkpoint(&mut self) {
                self.store.clear();
            }

            $v fn new() -> Self {
                Self::default()
            }
        }
    }
}

/// Common implementation for expectations that return 'static variables
#[doc(hidden)]
#[macro_export]
macro_rules! static_expectation {
    (
        $v:vis [$($generics:ident, )*] [$($args:ident),*] [$($argty:ty),*]
        [$($altargs:ident),*] [$($matchty:ty),*] [$($matchcall:expr),*] $o:ty
    ) => {
        use ::downcast::*;
        use ::fragile::Fragile;
        use ::predicates_tree::CaseTreeExt;
        use ::std::{
            collections::hash_map::HashMap,
            marker::PhantomData,
            mem,
            ops::{DerefMut, Range},
            sync::{Mutex, MutexGuard}
        };
        use super::*;   // Import types from the calling environment

        enum Rfunc<$($generics: 'static,)*> {
            Default,
            // Indicates that a `return_once` expectation has already returned
            Expired,
            Mut(Box<dyn FnMut($( $argty, )*) -> $o + Send>),
            // Should be Box<dyn FnOnce> once that feature is stabilized
            // https://github.com/rust-lang/rust/issues/28796
            Once(Box<dyn FnMut($( $argty, )*) -> $o + Send>),
            // Prevent "unused type parameter" errors
            // Surprisingly, PhantomData<Fn(generics)> is Send even if generics
            // are not, unlike PhantomData<generics>
            _Phantom(Box<Fn($($generics,)*) -> () + Send>)
        }

        impl<$($generics,)*>  Rfunc<$($generics,)*> {
            fn call_mut(&mut self, $( $args: $argty, )* ) -> $o {
                match self {
                    Rfunc::Default => {
                        use $crate::ReturnDefault;
                        $crate::DefaultReturner::<$o>::return_default()
                    },
                    Rfunc::Expired => {
                        panic!("Called a method twice that was expected only once")
                    },
                    Rfunc::Mut(f) => {
                        f( $( $args, )* )
                    },
                    Rfunc::Once(_) => {
                        let fo = mem::replace(self, Rfunc::Expired);
                        if let Rfunc::Once(mut f) = fo {
                            f( $( $args, )* )
                        } else {
                            unreachable!()
                        }
                    },
                    Rfunc::_Phantom(_) => unreachable!()
                }
            }
        }

        impl<$($generics,)*>
            std::default::Default for Rfunc<$($generics,)*>
        {
            fn default() -> Self {
                Rfunc::Default
            }
        }

        $crate::common_matcher!{
            [$($generics, )*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        $crate::common_methods!{
            [$($generics, )*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        /// Expectation type for methods that return a `'static` type.
        /// This is the type returned by the `expect_*` methods.
        $v struct Expectation<$($generics: 'static,)*> {
            common: Common<$($generics,)*>,
            rfunc: Mutex<Rfunc<$($generics,)*>>,
        }

        impl<$($generics,)*> Expectation<$($generics,)*> {
            /// Call this [`Expectation`] as if it were the real method.
            #[doc(hidden)]
            $v fn call(&self, $( $args: $argty, )* ) -> $o
            {
                self.common.call($( $matchcall, )*);
                self.rfunc.lock().unwrap().call_mut($( $args, )*)
            }

            /// Return a constant value from the `Expectation`
            ///
            /// The output type must be `Clone`.  The compiler can't always
            /// infer the proper type to use with this method; you will usually
            /// need to specify it explicitly.  i.e. `return_const(42i32)`
            /// instead of `return_const(42)`.
            // We must use Into<$o> instead of $o because where clauses don't
            // accept equality constraints.
            // https://github.com/rust-lang/rust/issues/20041
            #[allow(unused_variables)]
            $v fn return_const<MockallOutput>(&mut self, c: MockallOutput) -> &mut Self
                where MockallOutput: Clone + Into<$o> + Send + 'static
            {
                let f = move |$( $args: $argty, )*| c.clone().into();
                self.returning(f)
            }

            /// Supply an `FnOnce` closure that will provide the return value
            /// for this Expectation.  This is useful for return types that
            /// aren't `Clone`.  It will be an error to call this method
            /// multiple times.
            $v fn return_once<F>(&mut self, f: F) -> &mut Self
                where F: FnOnce($( $argty, )*) -> $o + Send + 'static
            {
                let mut fopt = Some(f);
                let fmut = move |$( $args: $argty, )*| {
                    if let Some(f) = fopt.take() {
                        f($( $args, )*)
                    } else {
                        panic!("Called a method twice that was expected only once")
                    }
                };
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(),
                                 Rfunc::Once(Box::new(fmut)));
                }
                self
            }

            /// Single-threaded version of [`return_once`](#method.return_once).
            /// This is useful for return types that are neither `Send` nor
            /// `Clone`.
            ///
            /// It is a runtime error to call the mock method from a different
            /// thread than the one that originally called this method.  It is
            /// also a runtime error to call the method more than once.
            $v fn return_once_st<F>(&mut self, f: F) -> &mut Self
                where F: FnOnce($( $argty, )*) -> $o + 'static
            {
                let mut fragile = Some(::fragile::Fragile::new(f));
                let fmut = Box::new(move |$( $args: $argty, )*| {
                    match fragile.take() {
                        Some(frag) => (frag.into_inner())($( $args, )*),
                        None => panic!(
                            "Called a method twice that was expected only once")
                    }
                });
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(), Rfunc::Once(fmut));
                }
                self
            }

            /// Supply a closure that will provide the return value for this
            /// `Expectation`.  The method's arguments are passed to the closure
            /// by value.
            $v fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $argty, )*) -> $o + Send + 'static
            {
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
                }
                self
            }

            /// Single-threaded version of [`returning`](#method.returning).
            /// Can be used when the argument or return type isn't `Send`.
            ///
            /// It is a runtime error to call the mock method from a different
            /// thread than the one that originally called this method.
            $v fn returning_st<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $argty, )*) -> $o + 'static
            {
                let mut fragile = Fragile::new(f);
                let fmut = move |$( $args: $argty, )*| {
                    (fragile.get_mut())($( $args, )*)
                };
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(fmut)));
                }
                self
            }

            $crate::expectation_methods!{
                $v [$($args)*] [$($altargs)*] [$($matchty)*]
            }
        }
        impl<$($generics,)*> Default for Expectation<$($generics,)*>
        {
            fn default() -> Self {
                Expectation {
                    common: Common::default(),
                    rfunc: Mutex::new(Rfunc::default())
                }
            }
        }

        $crate::expectations_methods!{$v [$($generics, )*]}
        impl<$($generics,)*> Expectations<$($generics,)*> {
            /// Simulating calling the real method.  Every current expectation
            /// will be checked in FIFO order and the first one with matching
            /// arguments will be used.
            $v fn call(&self, $( $args: $argty, )* ) -> $o
            {
                let n = self.0.len();
                match self.0.iter()
                    .find(|e| e.matches($( $matchcall, )*) &&
                          (!e.is_done() || n == 1))
                {
                    None => panic!("No matching expectation found"),
                    Some(e) => e.call($( $args, )*)
                }
            }

        }
        impl<$($generics: 'static,)*>
            $crate::AnyExpectations for Expectations<$($generics,)*>
        {}

        $crate::generic_expectation_methods!{$v [$($generics, )*] [$($argty)*] $o}
        impl GenericExpectations {
            /// Simulating calling the real method.
            $v fn call<$($generics: 'static,)*>
                (&self, $( $args: $argty, )* ) -> $o
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let e: &Expectations<$($generics,)*> = self.store.get(&key)
                    .expect("No matching expectation found")
                    .downcast_ref()
                    .unwrap();
                e.call($( $args, )*)
            }

            /// Create a new Expectation.
            $v fn expect<'e, $($generics: 'static,)*>
                (&'e mut self)
                -> &'e mut Expectation<$($generics,)*>
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.store.entry(key)
                    .or_insert_with(||
                        Box::new(Expectations::<$($generics,)*>::new())
                    ).downcast_mut()
                    .unwrap();
                ee.expect()
            }

        }
    }
}
