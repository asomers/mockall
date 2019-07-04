// vim: tw=80

/// The Matcher class is common to all Expectation types
#[macro_export]
#[doc(hidden)]
macro_rules! common_matcher {
    ([$($generics:ident)*] [$($args:ident)*]
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
    ([$($generics:ident)*] [$($args:ident)*]
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
    ($v:vis [$($generics:ident)*]) => {
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
    ($v:vis [$($generics:ident)*] [$($argty:ty)*] $o:ty) =>
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
        $v:vis [$($generics:ident)*] [$($args:ident)*] [$($argty:ty)*]
        [$($altargs:ident)*] [$($matchty:ty)*] [$($matchcall:expr)*] $o:ty
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
            [$($generics)*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        $crate::common_methods!{
            [$($generics)*] [$($args)*] [$($altargs)*] [$($matchty)*]
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

        $crate::expectations_methods!{$v [$($generics)*]}
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

        $crate::generic_expectation_methods!{$v [$($generics)*] [$($argty)*] $o}
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

/// Generate Expectation and Expectations types for a single method.
///
/// This macro can mock most method types, whether they take `self`, `&self`, or
/// `&mut self` references, `'static` arguments or reference arguments, and
/// `'static` or reference return types.
///
/// Reference arguments will be given independent anonymous lifetimes.  This is
/// the usual behavior for methods that don't specify explicit lifetimes.
///
/// Methods that return references will use `self`'s lifetime for the lifetime
/// of the returned value.
///
/// Generic methods are allowed, as long as the generic parameters are `'static`
/// (but concrete types don't need to be).  Don't repeat the generic bounds in
/// the macro invocation.
///
/// # Arguments
///
/// * `vis`:            Visibility qualifier relative to `expectation!`'s
///                     private module.  Private visibility is not really
///                     useful.
/// * `module`:         Name of the module to create
/// * `generics`:       Comma-delimited sequence of generic parameters, sans
///                     bounds.
/// * `o`:              Owned version of the output type.  Must be a `'static`.
///                     The real output type will be a reference to this one.
///                     Returning references, both mutable and immutable, is
///                     allowed, but note that the `&` or `&mut` is technically
///                     not part of the macro argument.
/// * `argty`:          Comma-delimited sequence of arguments types for the
///                     method being mocked.
/// * `matchcall`:      Comma-delimited sequence of expressions that produce
///                     values of type `matchty` from values of type `argty`.
/// * `args`:           comma-delimited sequence of argument names for each
///                     argument.  Ideally this wouldn't need to be specified,
///                     but it is due to Rust's macro hygiene rules.
/// * `altargs`:        Comma-delimited sequence of identifiers of the same
///                     length as `args`, but distinct.
/// * `matchty`:        comma-delimited sequence of types for each match
///                     argument.  Must all be `'static`.
///
/// # Examples
///
/// Mock a method with a `'static` return type like
/// `foo(&self, x: u32, y: &i16) -> u32`
/// ```no_run
/// # use mockall::*;
/// expectation! {
///     pub fn foo<>(&self, x: u32, y: &i16) -> u32 {
///         let (px: &u32, py: &i16) = (&x, y);
///     }
/// }
/// ```
///
/// Mock a generic method with a `'static` return type like
/// `foo<D: Clone>(d: D, x: &u32) -> bool`
/// ```no_run
/// # use mockall::*;
/// expectation! {
///     pub fn foo<D>(&self, d: D, x: &u32) -> bool {
///         let (pd: &D, px: &u32) = (&d, x);
///     }
/// }
/// ```
///
/// Mock a method returning a reference like
/// `foo(&self, x: u32, y: &i16) -> &u32`
/// ```no_run
/// # use mockall::*;
/// expectation!{
///     pub fn foo<>(&self, i0: u32, i1: &i16) -> &u32 {
///         let (p0: &u32, p1: &i16) = (&i0, i1);
///     }
/// }
/// ```
///
/// Mock a method returning a mutable reference like
/// `foo(&mut self, x: u32, y: &i16) -> &mut u32`
/// ```no_run
/// # use mockall::*;
/// expectation!{
///     pub fn foo<>(&mut self, i0: u32, i1: &i16) -> &mut u32 {
///         let (p0: &u32, p1: &i16) = (&i0, i1);
///     }
/// }
/// ```
#[macro_export]
macro_rules! expectation {
    (
        // First pattern, for methods returning 'static references
        //pub fn foo<>(&self, )
        $v:vis fn $module:ident < $( $generics:ident ),* >
        ($(&)?$(mut)?self $(,)? $( $args:ident : $argty:ty ),* )
            -> & 'static $o:ty
        {
            let ( $( $altargs:ident : &$matchty:ty ),* ) =
                ( $( $matchcall:expr ),* );
        }
    ) => {
        pub mod $module {
            $crate::static_expectation!{
                $v [$($generics)*] [$($args)*] [$($argty)*] [$($altargs)*]
                [$($matchty)*] [$($matchcall)*] &'static $o
            }
        }
    };
    (
        // Second pattern, for methods taking &self and returning immutable
        // references.
        $v:vis fn $module:ident
        < $( $generics:ident ),* >
        (&self $(,)? $( $args:ident : $argty:ty ),* ) -> & $o:ty
        {
            let ( $( $altargs:ident : &$matchty:ty ),* ) =
                ( $( $matchcall:expr ),* );
        }
    ) => {
        pub mod $module {
        use ::downcast::*;
        use ::fragile::Fragile;
        use ::predicates_tree::CaseTreeExt;
        use ::std::{
            collections::hash_map::HashMap,
            marker::PhantomData,
            mem,
            ops::{DerefMut, Range},
            sync::Mutex
        };
        use super::*;

        $crate::common_matcher!{
            [$($generics)*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        $crate::common_methods!{
            [$($generics)*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        /// Expectation type for methods taking a `&self` argument and returning
        /// immutable references.  This is the type returned by the `expect_*`
        /// methods.
        $v struct Expectation<$($generics: 'static,)*> {
            common: Common<$($generics,)*>,
            result: Option<$o>,
        }

        impl<$($generics: 'static,)*> Expectation<$($generics,)*> {
            $v fn call(&self, $( $args: $argty, )* ) -> &$o {
                self.common.call($( $matchcall, )*);
                &self.result.as_ref()
                    .expect("Must set return value with return_const")
            }

            /// Return a reference to a constant value from the `Expectation`
            $v fn return_const(&mut self, o: $o) -> &mut Self {
                self.result = Some(o);
                self
            }

            $crate::expectation_methods!{
                $v [$($args)*] [$($altargs)*] [$($matchty)*]
            }
        }

        impl<$($generics: 'static,)*> Default for Expectation<$($generics,)*>
        {
            fn default() -> Self {
                Expectation {
                    common: Common::default(),
                    result: None
                }
            }
        }

        $crate::expectations_methods!{$v [$($generics)*]}
        impl<$($generics: 'static,)*> Expectations<$($generics,)*> {
            /// Simulating calling the real method.  Every current expectation
            /// will be checked in FIFO order and the first one with matching
            /// arguments will be used.
            $v fn call(&self, $( $args: $argty, )* ) -> &$o {
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
        // The Senc + Sync are required for downcast, since Expectation stores
        // an Option<$o>
        impl<$($generics: 'static,)*>
            $crate::AnyExpectations for Expectations<$($generics,)*>
                where $o: Send + Sync
        {}

        $crate::generic_expectation_methods!{$v [$($generics)*] [$($argty)*] $o}
        impl GenericExpectations {
            /// Simulating calling the real method.
            $v fn call<$($generics: 'static,)*>
                (&self, $( $args: $argty, )* ) -> &$o
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let e: &Expectations<$($generics,)*> = self.store.get(&key)
                    .expect("No matching expectation found")
                    .downcast_ref()
                    .unwrap();
                e.call($( $args, )*)
            }

            /// Create a new Expectation.
            $v fn expect<'e, $($generics: 'static,)*>(&'e mut self)
                -> &'e mut Expectation<$($generics,)*>
                where $o: Send + Sync
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
    };

    (
        // Third pattern, for methods taking &mut self and returning mutable or
        // immutable references.
        $v:vis fn $module:ident
        < $( $generics:ident ),* >
        (&mut self $(,)? $( $args:ident : $argty:ty ),* ) -> & $(mut)? $o:ty
        {
            let ( $( $altargs:ident : &$matchty:ty ),* ) =
                ( $( $matchcall:expr ),* );
        }
    ) => {
        pub mod $module {
        use ::downcast::*;
        use ::predicates_tree::CaseTreeExt;
        use ::fragile::Fragile;
        use ::std::{
            collections::hash_map::HashMap,
            marker::PhantomData,
            mem,
            ops::{DerefMut, Range},
            sync::{Mutex, MutexGuard}
        };
        use super::*;

        $crate::common_matcher!{
            [$($generics)*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        $crate::common_methods!{
            [$($generics)*] [$($args)*] [$($altargs)*] [$($matchty)*]
        }

        /// Expectation type for methods taking a `&mut self` argument and
        /// returning references.  This is the type returned by the `expect_*`
        /// methods.
        $v struct Expectation<$($generics: 'static,)*> {
            common: Common<$($generics,)*>,
            result: Option<$o>,
            rfunc: Option<Box<dyn FnMut($( $argty, )*) -> $o + Send + Sync>>,
        }

        impl<$($generics: 'static,)*> Expectation<$($generics,)*> {
            /// Simulating calling the real method for this expectation
            $v fn call_mut(&mut self, $( $args: $argty, )*) -> &mut $o {
                self.common.call($( $matchcall, )*);
                if let Some(ref mut f) = self.rfunc {
                    self.result = Some(f($( $args, )*));
                }
                self.result.as_mut()
                    .expect("Must first set return function with returning or return_var")
            }

            /// Convenience method that can be used to supply a return value for
            /// a `Expectation`.  The value will be returned by mutable
            /// reference.
            $v fn return_var(&mut self, o: $o) -> &mut Self
            {
                self.result = Some(o);
                self
            }

            /// Supply a closure that the `Expectation` will use to create its
            /// return value.  The return value will be returned by mutable
            /// reference.
            $v fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $argty, )*) -> $o + Send + Sync + 'static
            {
                mem::replace(&mut self.rfunc, Some(Box::new(f)));
                self
            }

            /// Single-threaded version of [`returning`](#method.returning).
            /// Can be used when the argument or return type isn't `Send`.
            $v fn returning_st<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $argty, )*) -> $o + 'static
            {
                let mut fragile = Fragile::new(f);
                let fmut = move |$( $args: $argty, )*| {
                    (fragile.get_mut())($( $args, )*)
                };
                mem::replace(&mut self.rfunc, Some(Box::new(fmut)));
                self
            }

            $crate::expectation_methods!{
                $v [$($args)*] [$($altargs)*] [$($matchty)*]
            }
        }
        impl<$($generics: 'static,)*> Default for Expectation<$($generics,)*>
        {
            fn default() -> Self {
                Expectation {
                    common: Common::default(),
                    result: None,
                    rfunc: None
                }
            }
        }
        $crate::expectations_methods!{$v [$($generics)*]}
        impl<$($generics: 'static,)*> Expectations<$($generics,)*> {
            /// Simulating calling the real method.  Every current expectation
            /// will be checked in FIFO order and the first one with matching
            /// arguments will be used.
            $v fn call_mut(&mut self, $( $args: $argty, )* ) -> &mut $o {
                let n = self.0.len();
                match self.0.iter_mut()
                    .find(|e| e.matches($( $matchcall, )*) &&
                          (!e.is_done() || n == 1))
                {
                    None => panic!("No matching expectation found"),
                    Some(e) => e.call_mut($( $args, )*)
                }
            }
        }
        // The Senc + Sync are required for downcast, since Expectation stores
        // an Option<$o>
        impl<$($generics: 'static,)*>
            $crate::AnyExpectations for Expectations<$($generics,)*>
            where $o: Send + Sync
        {}

        $crate::generic_expectation_methods!{$v [$($generics)*] [$($argty)*] $o}
        impl GenericExpectations {
            /// Simulating calling the real method.
            $v fn call_mut<$($generics: 'static,)*>
                (&mut self, $( $args: $argty, )* ) -> &mut $o
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let e: &mut Expectations<$($generics,)*> = self.store
                    .get_mut(&key)
                    .expect("No matching expectation found")
                    .downcast_mut()
                    .unwrap();
                e.call_mut($( $args, )*)
            }

            /// Create a new Expectation.
            $v fn expect<'e, $($generics: 'static,)*> (&'e mut self)
                -> &'e mut Expectation<$($generics,)*>
                where $o: Send + Sync
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
    };

    (
        // Fourth pattern, for methods returning 'static values
        $v:vis fn $module:ident < $( $generics:ident ),* >
        ($(&)?$(mut)?self $(,)? $( $args:ident : $argty:ty ),* ) -> $o:ty
        {
            let ( $( $altargs:ident : &$matchty:ty ),* ) =
                ( $( $matchcall:expr ),* );
        }
    ) => {
        pub mod $module {
            $crate::static_expectation!{
                $v [$($generics)*] [$($args)*] [$($argty)*] [$($altargs)*]
                [$($matchty)*] [$($matchcall)*] $o
            }
        }
    };
    (
        // Fifth pattern, for static methods
        $v:vis fn $module:ident < $( $generics:ident ),* >
        ($( $args:ident : $argty:ty ),* ) -> $o:ty
        {
            let ( $( $altargs:ident : &$matchty:ty ),* ) =
                ( $( $matchcall:expr ),* );
        }
    ) => {
        pub mod $module {
        $crate::static_expectation!{
            $v [$($generics)*] [$($args)*] [$($argty)*] [$($altargs)*]
            [$($matchty)*] [$($matchcall)*] $o
        }

        /// Like an [`&Expectation`](struct.Expectation.html) but protected by a
        /// Mutex guard.  Useful for mocking static methods.  Forwards accesses
        /// to an `Expectation` object.
        // We must return the MutexGuard to the caller so he can configure the
        // expectation.  But we can't bundle both the guard and the &Expectation
        // into the same structure; the borrow checker won't let us.  Instead
        // we'll record the expectation's position within the Expectations
        // vector so we can proxy its methods.
        //
        // ExpectationGuard is only defined for expectations that return 'static
        // return types.
        $v struct ExpectationGuard<'guard, $($generics: 'static,)*>{
            guard: MutexGuard<'guard, Expectations<$($generics,)*>>,
            i: usize
        }

        impl<'guard, $($generics,)*> ExpectationGuard<'guard, $($generics,)*> {
            /// Just like
            /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
            $v fn in_sequence(&mut self, seq: &mut $crate::Sequence)
                -> &mut Expectation<$($generics,)*>
            {
                self.guard.0[self.i].in_sequence(seq)
            }

            /// Just like
            /// [`Expectation::never`](struct.Expectation.html#method.never)
            $v fn never(&mut self) -> &mut Expectation<$($generics,)*> {
                self.guard.0[self.i].never()
            }

            // Should only be called from the mockall_derive generated code
            #[doc(hidden)]
            $v fn new(mut guard: MutexGuard<'guard, Expectations<$($generics,)*>>)
                -> Self
            {
                guard.expect(); // Drop the &Expectation
                let i = guard.0.len() - 1;
                ExpectationGuard{guard, i}
            }

            /// Just like [`Expectation::once`](struct.Expectation.html#method.once)
            $v fn once(&mut self) -> &mut Expectation<$($generics,)*> {
                self.guard.0[self.i].once()
            }

            /// Just like
            /// [`Expectation::returning`](struct.Expectation.html#method.returning)
            $v fn returning<F>(&mut self, f: F)
                -> &mut Expectation<$($generics,)*>
                where F: FnMut($( $argty, )*) -> $o + Send + 'static
            {
                self.guard.0[self.i].returning(f)
            }

            /// Just like
            /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
            $v fn return_once<F>(&mut self, f: F)
                -> &mut Expectation<$($generics,)*>
                where F: FnOnce($( $argty, )*) -> $o + Send + 'static
            {
                self.guard.0[self.i].return_once(f)
            }

            /// Just like
            /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
            $v fn returning_st<F>(&mut self, f: F)
                -> &mut Expectation<$($generics,)*>
                where F: FnMut($( $argty, )*) -> $o + 'static
            {
                self.guard.0[self.i].returning_st(f)
            }

            /// Just like
            /// [`Expectation::times`](struct.Expectation.html#method.times)
            $v fn times(&mut self, n: usize)
                -> &mut Expectation<$($generics,)*> {
                self.guard.0[self.i].times(n)
            }

            /// Just like
            /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
            $v fn times_any(&mut self) -> &mut Expectation<$($generics,)*> {
                self.guard.0[self.i].times_any()
            }

            /// Just like
            /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
            $v fn times_range(&mut self, range: Range<usize>)
                -> &mut Expectation<$($generics,)*>
            {
                self.guard.0[self.i].times_range(range)
            }

            /// Just like
            /// [`Expectation::with`](struct.Expectation.html#method.with)
            #[allow(non_camel_case_types)]  // Repurpose $altargs for generics
            $v fn with<$( $altargs: $crate::Predicate<$matchty> + Send + 'static,)*>
                (&mut self, $( $args: $altargs,)*)
                -> &mut Expectation<$($generics,)*>
            {
                self.guard.0[self.i].with($($args,)*)
            }

            /// Just like
            /// [`Expectation::withf`](struct.Expectation.html#method.withf)
            $v fn withf<F>(&mut self, f: F) -> &mut Expectation<$($generics,)*>
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                self.guard.0[self.i].withf(f)
            }
        }

        /// Like a [`ExpectationGuard`](struct.ExpectationGuard.html) but for
        /// generic methods.
        $v struct GenericExpectationGuard<'guard, $($generics: 'static,)*> {
            guard: MutexGuard<'guard, GenericExpectations>,
            i: usize,
            _phantom: PhantomData<((), $($generics,)*)>,
        }

        impl<'guard, $($generics: 'static,)*>
            GenericExpectationGuard<'guard, $($generics,)*>
        {
            /// Just like
            /// [`Expectation::in_sequence`](struct.Expectation.html#method.in_sequence)
            $v fn in_sequence(&mut self, seq: &mut $crate::Sequence)
                -> &mut Expectation<$($generics,)*>
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].in_sequence(seq)
            }

            /// Just like
            /// [`Expectation::never`](struct.Expectation.html#method.never)
            $v fn never(&mut self) -> &mut Expectation<$($generics,)*> {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].never()
            }

            // Should only be called from the mockall_derive generated code
            #[doc(hidden)]
            $v fn new(mut guard: MutexGuard<'guard, GenericExpectations>)
                -> Self
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    guard.store.entry(key)
                    .or_insert_with(||
                        Box::new(Expectations::<$($generics,)*>::new()))
                    .downcast_mut()
                    .unwrap();
                ee.expect();    // Drop the &Expectation
                let i = ee.0.len() - 1;
                GenericExpectationGuard{guard, i, _phantom: PhantomData}
            }

            /// Just like
            /// [`Expectation::once`](struct.Expectation.html#method.once)
            $v fn once(&mut self) -> &mut Expectation<$($generics,)*> {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].once()
            }

            /// Just like
            /// [`Expectation::returning`](struct.Expectation.html#method.returning)
            $v fn returning<F>(&mut self, f: F) -> &mut Expectation<$($generics,)*>
                where F: FnMut($( $argty, )*) -> $o + Send + 'static
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].returning(f)
            }

            /// Just like
            /// [`Expectation::return_once`](struct.Expectation.html#method.return_once)
            $v fn return_once<F>(&mut self, f: F) -> &mut Expectation<$($generics,)*>
                where F: FnOnce($( $argty, )*) -> $o + Send + 'static
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].return_once(f)
            }

            /// Just like
            /// [`Expectation::returning_st`](struct.Expectation.html#method.returning_st)
            $v fn returning_st<F>(&mut self, f: F) -> &mut Expectation<$($generics,)*>
                where F: FnMut($( $argty, )*) -> $o + 'static
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].returning_st(f)
            }

            /// Just like
            /// [`Expectation::times`](struct.Expectation.html#method.times)
            $v fn times(&mut self, n: usize) -> &mut Expectation<$($generics,)*> {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].times(n)
            }

            /// Just like
            /// [`Expectation::times_any`](struct.Expectation.html#method.times_any)
            $v fn times_any(&mut self) -> &mut Expectation<$($generics,)*> {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].times_any()
            }

            /// Just like
            /// [`Expectation::times_range`](struct.Expectation.html#method.times_range)
            $v fn times_range(&mut self, range: Range<usize>) -> &mut Expectation<$($generics,)*>
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].times_range(range)
            }

            /// Just like
            /// [`Expectation::with`](struct.Expectation.html#method.with)
            #[allow(non_camel_case_types)]  // Repurpose $altargs for generics
            $v fn with<$( $altargs: $crate::Predicate<$matchty> + Send + 'static,)*>
                (&mut self, $( $args: $altargs,)*)
                -> &mut Expectation<$($generics,)*>
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].with($($args,)*)
            }

            /// Just like
            /// [`Expectation::withf`](struct.Expectation.html#method.withf)
            $v fn withf<F>(&mut self, f: F) -> &mut Expectation<$($generics,)*>
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                let key = $crate::Key::new::<($($argty,)*)>();
                let ee: &mut Expectations<$($generics,)*> =
                    self.guard.store.get_mut(&key).unwrap()
                    .downcast_mut().unwrap();
                ee.0[self.i].withf(f)
            }
        }
        }
    };

}
