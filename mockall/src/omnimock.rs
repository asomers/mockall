// vim: tw=80

/// Common methods of the Common struct
// Ideally methods like with and withf that are identical for all
// Expectation types would be defined here, too.  But that's not possible for
// methods that need any of the variable-length argument fields due to this Rust
// bug:
// https://github.com/rust-lang/rust/issues/35853
#[macro_export]
#[doc(hidden)]
macro_rules! common_methods {
    () => {
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

        /// Forbid this expectation from ever being called
        fn never(&mut self) {
            self.times.never();
        }

        fn satisfy_sequence(&self) {
            if let Some(handle) = &self.seq_handle {
                handle.satisfy()
            }
        }

        /// Require this expectation to be called exactly `n` times.
        fn times(&mut self, n: usize) {
            self.times.n(n);
        }

        /// Allow this expectation to be called any number of times
        fn times_any(&mut self) {
            self.times.any();
        }

        /// Allow this expectation to be called any number of times within a
        /// given range
        fn times_range(&mut self, range: ::std::ops::Range<usize>) {
            self.times.range(range);
        }

        fn verify_sequence(&self) {
            if let Some(handle) = &self.seq_handle {
                handle.verify()
            }
        }
    }
}

/// Common methods of the Expectation, RefExpectation, and RefMutExpectation
/// structs
// Rust bug 35853 applies here, too.
#[macro_export]
#[doc(hidden)]
macro_rules! expectation_methods {
    () => {
        /// Add this expectation to a [`Sequence`](struct.Sequence.html).
        pub fn in_sequence(&mut self, seq: &mut $crate::Sequence) -> &mut Self {
            self.common.in_sequence(seq);
            self
        }

        pub fn is_done(&self) -> bool {
            self.common.is_done()
        }

        /// Forbid this expectation from ever being called
        pub fn never(&mut self) -> &mut Self {
            self.common.never();
            self
        }

        /// Expect this expectation to be called exactly once.  Shortcut for
        /// [`times(1)`](#method.times).
        pub fn once(&mut self) -> &mut Self {
            self.times(1)
        }

        /// Require this expectation to be called exactly `n` times.
        pub fn times(&mut self, n: usize) -> &mut Self {
            self.common.times(n);
            self
        }

        /// Allow this expectation to be called any number of times
        ///
        /// This behavior is the default, but the method is provided in case the
        /// default behavior changes.
        pub fn times_any(&mut self) -> &mut Self {
            self.common.times_any();
            self
        }

        /// Allow this expectation to be called any number of times within a
        /// given range
        pub fn times_range(&mut self, range: ::std::ops::Range<usize>)
            -> &mut Self
        {
            self.common.times_range(range);
            self
        }
    }
}

/// Generate Expectation and Expectations types with specific signatures, for
/// `'static` return types.
///
/// # Arguments
///
/// * `expectation`:    Name of the generated Expectation type
/// * `module`:         Name of the private module to create
/// * `o`:              Output type.  Must be static
/// * `methty`:         Comma-delimited sequence of arguments types for the
///                     method being mocked.
/// * `matchcall`:      Comma-delimited sequence of expressions that produce
///                     values of type `matchty` from values of type `methty`.
/// * `args`:           comma-delimited sequence of argument names for each
///                     argument.  Ideally this wouldn't need to be specified,
///                     but it is due to Rust's macro hygiene rules.
/// * `predargs`:       Comma-delimited sequence of identifiers of the same
///                     length as `args`, but distinct.
/// * `matchty`:        comma-delimited sequence of types for each match
///                     argument.  Must all be `'static`.
///
/// # Examples
///
/// ```no_run
/// # use mockall::*;
/// // Mock a method like foo(&self, x: u32, y: &i16) -> u32
/// expectation! {
///     FooExpectation, __foo__priv, u32, [u32, &i16],
///     [&i0, i1], [i0, i1], [p0, p1], [u32, i16]
/// }
/// ```
#[macro_export]
macro_rules! expectation {(
        $expectation:ident,
        $module:ident,
        $o:ty,
        [ $( $methty:ty ),* ],
        [ $( $matchcall:expr ),* ],
        [ $( $args:ident ),* ],
        [ $( $predargs:ident ),* ],
        [ $( $matchty:ty ),* ]) =>
    {
        mod $module {
        use ::predicates_tree::CaseTreeExt;
        use ::std::ops::DerefMut;
        use super::*;

        enum Rfunc {
            Default,
            // Indicates that a `return_once` expectation has already returned
            Expired,
            Mut(Box<dyn FnMut($( $methty, )*) -> $o + Send>),
            // Should be Box<dyn FnOnce> once that feature is stabilized
            // https://github.com/rust-lang/rust/issues/28796
            Once(Box<dyn FnMut($( $methty, )*) -> $o + Send>),
        }

        impl  Rfunc {
            fn call_mut(&mut self, $( $args: $methty, )* ) -> $o {
                match self {
                    Rfunc::Default => {
                        unimplemented!()
                        //Self::return_default()
                    },
                    Rfunc::Expired => {
                        panic!("Called a method twice that was expected only once")
                    },
                    Rfunc::Mut(f) => {
                        f( $( $args, )* )
                    },
                    Rfunc::Once(_) => {
                        let fo = ::std::mem::replace(self, Rfunc::Expired);
                        if let Rfunc::Once(mut f) = fo {
                            f( $( $args, )* )
                        } else {
                            unreachable!()
                        }
                    },
                }
            }
        }

        impl std::default::Default for Rfunc {
            fn default() -> Self {
                Rfunc::Default
            }
        }

        enum Matcher {
            Func(Box<Fn($( &$matchty, )* ) -> bool>),
            Pred( $( Box<$crate::Predicate<$matchty>>, )* )
        }

        impl Matcher {
            fn eval(&self, $( $args: &$matchty, )*) -> bool {
                match self {
                    Matcher::Func(f) => f($( $args, )*),
                    Matcher::Pred($( $predargs, )*) =>
                        [$( $predargs.eval($args), )*]
                        .into_iter()
                        .all(|x| *x),
                }
            }

            fn verify(&self, $( $args: &$matchty, )* ) {
                match self {
                    Matcher::Func(f) => assert!(f($( $args, )*),
                        "Expectation didn't match arguments"),
                    Matcher::Pred($( $predargs, )*) => {
                        $(if let Some(c) = $predargs.find_case(false, $args) {
                            panic!("Expectation didn't match arguments:\n{}",
                                   c.tree());
                        })*
                    }
                }
            }
        }

        impl Default for Matcher {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|$( $args, )*| true))
            }
        }

        /// Holds the stuff that is independent of the output type
        #[derive(Default)]
        struct Common {
            matcher: ::std::sync::Mutex<Matcher>,
            seq_handle: Option<$crate::SeqHandle>,
            times: $crate::Times
        }

        impl Common {
            fn call(&self, $( $args: &$matchty, )* ) {
                self.matcher.lock().unwrap().verify($( $args, )*);
                self.times.call();
                self.verify_sequence();
                if self.times.is_satisfied() {
                    self.satisfy_sequence()
                }
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            fn with<$( $predargs: $crate::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*)
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred($( Box::new($args), )*);
                ::std::mem::replace(guard.deref_mut(), m);
            }

            fn withf<F>(&mut self, f: F)
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Func(Box::new(f));
                ::std::mem::replace(guard.deref_mut(), m);
            }

            $crate::common_methods!{}
        }

        #[derive(Default)]
        pub struct Expectation {
            common: Common,
            rfunc: ::std::sync::Mutex<Rfunc>,
        }

        impl Expectation {
            pub fn call(&self, $( $args: $methty, )* ) -> $o {
                self.common.call($( $matchcall, )*);
                self.rfunc.lock().unwrap().call_mut($( $args, )*)
            }

            /// Supply an `FnOnce` closure that will provide the return value
            /// for this Expectation.  This is useful for return types that
            /// aren't `Clone`.  It will be an error to call this Expectation
            /// multiple times.
            pub fn return_once<F>(&mut self, f: F) -> &mut Self
                where F: FnOnce($( $methty, )*) -> $o + Send + 'static
            {
                let mut fopt = Some(f);
                let fmut = move |$( $args: $methty, )*| {
                    if let Some(f) = fopt.take() {
                        f($( $args, )*)
                    } else {
                        panic!("Called a method twice that was expected only once")
                    }
                };
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(),
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
            pub fn return_once_st<F>(&mut self, f: F) -> &mut Self
                where F: FnOnce($( $methty, )*) -> $o + 'static
            {
                let mut fragile = Some(::fragile::Fragile::new(f));
                let fmut = Box::new(move |$( $args: $methty, )*| {
                    match fragile.take() {
                        Some(frag) => (frag.into_inner())($( $args, )*),
                        None => panic!(
                            "Called a method twice that was expected only once")
                    }
                });
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(), Rfunc::Once(fmut));
                }
                self
            }

            pub fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $methty, )*) -> $o + Send + 'static
            {
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
                }
                self
            }

            /// Single-threaded version of [`returning`](#method.returning).
            /// Can be used when the argument or return type isn't `Send`.
            ///
            /// It is a runtime error to call the mock method from a different
            /// thread than the one that originally called this method.
            pub fn returning_st<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $methty, )*) -> $o + 'static
            {
                let mut fragile = ::fragile::Fragile::new(f);
                let fmut = move |$( $args: $methty, )*| {
                    (fragile.get_mut())($( $args, )*)
                };
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(),
                                        Rfunc::Mut(Box::new(fmut)));
                }
                self
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            pub fn with<$( $predargs: $crate::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*) -> &mut Self
            {
                self.common.with($( $args, )*);
                self
            }

            pub fn withf<F>(&mut self, f: F) -> &mut Self
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                self.common.withf(f);
                self
            }

            $crate::expectation_methods!{}
        }

        }
        use $module::Expectation as $expectation;
    }
}

/// Generate Expectation and Expectations types with specific signatures, for
/// methods that take `&self` arguments and return references with the same
/// lifetime as `self`.
///
/// # Arguments
///
/// * `expectation`:    Name of the generated Expectation type
/// * `module`:         Name of the private module to create
/// * `o`:              Owned version of the output type.  Must be a `'static`.
///                     The real output type will be a reference to this one.
/// * `methty`:         Comma-delimited sequence of arguments types for the
///                     method being mocked.
/// * `matchcall`:      Comma-delimited sequence of expressions that produce
///                     values of type `matchty` from values of type `methty`.
/// * `args`:           comma-delimited sequence of argument names for each
///                     argument.  Ideally this wouldn't need to be specified,
///                     but it is due to Rust's macro hygiene rules.
/// * `predargs`:       Comma-delimited sequence of identifiers of the same
///                     length as `args`, but distinct.
/// * `matchty`:        comma-delimited sequence of types for each match
///                     argument.  Must all be `'static`.
///
/// # Examples
///
/// ```no_run
/// # use mockall::*;
/// // Mock a method like foo(&self, x: u32, y: &i16) -> &u32
/// ref_expectation!{
///     FooExpectation, __foo__priv, u32, [u32, &i16],
///     [&i0, i1], [i0, i1], [p0, p1], [u32, i16]
/// }
/// ```
#[macro_export]
macro_rules! ref_expectation {(
        $expectation:ident,
        $module:ident,
        $o:ty,
        [ $( $methty:ty ),* ],
        [ $( $matchcall:expr ),* ],
        [ $( $args:ident ),* ],
        [ $( $predargs:ident ),* ],
        [ $( $matchty:ty ),* ]) =>
    {
        mod $module {
        use ::predicates_tree::CaseTreeExt;
        use ::std::ops::DerefMut;
        use super::*;

        enum Matcher {
            Func(Box<Fn($( &$matchty, )* ) -> bool>),
            Pred( $( Box<$crate::Predicate<$matchty>>, )* )
        }

        impl Matcher {
            fn eval(&self, $( $args: &$matchty, )*) -> bool {
                match self {
                    Matcher::Func(f) => f($( $args, )*),
                    Matcher::Pred($( $predargs, )*) =>
                        [$( $predargs.eval($args), )*]
                        .into_iter()
                        .all(|x| *x),
                }
            }

            fn verify(&self, $( $args: &$matchty, )* ) {
                match self {
                    Matcher::Func(f) => assert!(f($( $args, )*),
                        "Expectation didn't match arguments"),
                    Matcher::Pred($( $predargs, )*) => {
                        $(if let Some(c) = $predargs.find_case(false, $args) {
                            panic!("Expectation didn't match arguments:\n{}",
                                   c.tree());
                        })*
                    }
                }
            }
        }

        impl Default for Matcher {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|$( $args, )*| true))
            }
        }

        /// Holds the stuff that is independent of the output type
        #[derive(Default)]
        struct Common {
            matcher: ::std::sync::Mutex<Matcher>,
            seq_handle: Option<$crate::SeqHandle>,
            times: $crate::Times
        }

        impl Common {
            fn call(&self, $( $args: &$matchty, )* ) {
                self.matcher.lock().unwrap().verify($( $args, )*);
                self.times.call();
                self.verify_sequence();
                if self.times.is_satisfied() {
                    self.satisfy_sequence()
                }
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            fn with<$( $predargs: $crate::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*)
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred($( Box::new($args), )*);
                ::std::mem::replace(guard.deref_mut(), m);
            }

            fn withf<F>(&mut self, f: F)
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Func(Box::new(f));
                ::std::mem::replace(guard.deref_mut(), m);
            }

            $crate::common_methods!{}
        }

        #[derive(Default)]
        pub struct Expectation {
            common: Common,
            result: Option<$o>,
        }

        impl Expectation {
            pub fn call(&self, $( $args: $methty, )* ) -> &$o {
                self.common.call($( $matchcall, )*);
                &self.result.as_ref()
                    .expect("Must set return value with return_const")
            }

            pub fn new() -> Self {
                Self::default()
            }

            /// Return a reference to a constant value from the `Expectation`
            pub fn return_const(&mut self, o: $o) -> &mut Self {
                self.result = Some(o);
                self
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            pub fn with<$( $predargs: $crate::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*) -> &mut Self
            {
                self.common.with($( $args, )*);
                self
            }

            pub fn withf<F>(&mut self, f: F) -> &mut Self
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                self.common.withf(f);
                self
            }

            $crate::expectation_methods!{}
        }

        }
        use $module::Expectation as $expectation;
    }
}

/// Generate Expectation and Expectations types with specific signatures, for
/// methods that take `&mut self` arguments and return references with the same
/// lifetime as `self`.
///
/// # Arguments
///
/// * `expectation`:    Name of the generated Expectation type
/// * `module`:         Name of the private module to create
/// * `o`:              Owned version of the output type.  Must be a `'static`.
///                     The real output type will be a reference to this one.
/// * `methty`:         Comma-delimited sequence of arguments types for the
///                     method being mocked.
/// * `matchcall`:      Comma-delimited sequence of expressions that produce
///                     values of type `matchty` from values of type `methty`.
/// * `args`:           comma-delimited sequence of argument names for each
///                     argument.  Ideally this wouldn't need to be specified,
///                     but it is due to Rust's macro hygiene rules.
/// * `predargs`:       Comma-delimited sequence of identifiers of the same
///                     length as `args`, but distinct.
/// * `matchty`:        comma-delimited sequence of types for each match
///                     argument.  Must all be `'static`.
///
/// # Examples
///
/// ```no_run
/// # use mockall::*;
/// // Mock a method like foo(&mut self, x: u32, y: &i16) -> &mut u32
/// ref_mut_expectation!{
///     FooExpectation, __foo__priv, u32, [u32, &i16],
///     [&i0, i1], [i0, i1], [p0, p1], [u32, i16]
/// }
/// ```
#[macro_export]
macro_rules! ref_mut_expectation {(
        $expectation:ident,
        $module:ident,
        $o:ty,
        [ $( $methty:ty ),* ],
        [ $( $matchcall:expr ),* ],
        [ $( $args:ident ),* ],
        [ $( $predargs:ident ),* ],
        [ $( $matchty:ty ),* ]) =>
    {
        mod $module {
        use ::predicates_tree::CaseTreeExt;
        use ::std::ops::DerefMut;
        use super::*;

        enum Matcher {
            Func(Box<Fn($( &$matchty, )* ) -> bool>),
            Pred( $( Box<$crate::Predicate<$matchty>>, )* )
        }

        impl Matcher {
            fn eval(&self, $( $args: &$matchty, )*) -> bool {
                match self {
                    Matcher::Func(f) => f($( $args, )*),
                    Matcher::Pred($( $predargs, )*) =>
                        [$( $predargs.eval($args), )*]
                        .into_iter()
                        .all(|x| *x),
                }
            }

            fn verify(&self, $( $args: &$matchty, )* ) {
                match self {
                    Matcher::Func(f) => assert!(f($( $args, )*),
                        "Expectation didn't match arguments"),
                    Matcher::Pred($( $predargs, )*) => {
                        $(if let Some(c) = $predargs.find_case(false, $args) {
                            panic!("Expectation didn't match arguments:\n{}",
                                   c.tree());
                        })*
                    }
                }
            }
        }

        impl Default for Matcher {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|$( $args, )*| true))
            }
        }

        /// Holds the stuff that is independent of the output type
        #[derive(Default)]
        struct Common {
            matcher: ::std::sync::Mutex<Matcher>,
            seq_handle: Option<$crate::SeqHandle>,
            times: $crate::Times
        }

        impl Common {
            fn call(&self, $( $args: &$matchty, )* ) {
                self.matcher.lock().unwrap().verify($( $args, )*);
                self.times.call();
                self.verify_sequence();
                if self.times.is_satisfied() {
                    self.satisfy_sequence()
                }
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            fn with<$( $predargs: $crate::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*)
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred($( Box::new($args), )*);
                ::std::mem::replace(guard.deref_mut(), m);
            }

            fn withf<F>(&mut self, f: F)
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Func(Box::new(f));
                ::std::mem::replace(guard.deref_mut(), m);
            }

            $crate::common_methods!{}
        }

        #[derive(Default)]
        pub struct Expectation {
            common: Common,
            result: Option<$o>,
            rfunc: Option<Box<dyn FnMut($( $methty, )*) -> $o + Send + Sync>>,
        }

        impl Expectation {
            /// Simulating calling the real method for this expectation
            pub fn call_mut(&mut self, $( $args: $methty, )*) -> &mut $o {
                self.common.call($( $matchcall, )*);
                if let Some(ref mut f) = self.rfunc {
                    self.result = Some(f($( $args, )*));
                }
                self.result.as_mut()
                    .expect("Must first set return function with returning or return_var")
            }

            pub fn new() -> Self {
                Self::default()
            }

            /// Convenience method that can be used to supply a return value for
            /// a `Expectation`.  The value will be returned by mutable
            /// reference.
            pub fn return_var(&mut self, o: $o) -> &mut Self
            {
                self.result = Some(o);
                self
            }

            /// Supply a closure that the `Expectation` will use to create its
            /// return value.  The return value will be returned by mutable
            /// reference.
            pub fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $methty, )*) -> $o + Send + Sync + 'static
            {
                ::std::mem::replace(&mut self.rfunc, Some(Box::new(f)));
                self
            }

            /// Single-threaded version of [`returning`](#method.returning).
            /// Can be used when the argument or return type isn't `Send`.
            pub fn returning_st<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $methty, )*) -> $o + 'static
            {
                let mut fragile = ::fragile::Fragile::new(f);
                let fmut = move |$( $args, )*| {
                    (fragile.get_mut())($( $args, )*)
                };
                ::std::mem::replace(&mut self.rfunc, Some(Box::new(fmut)));
                self
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            pub fn with<$( $predargs: $crate::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*) -> &mut Self
            {
                self.common.with($( $args, )*);
                self
            }

            pub fn withf<F>(&mut self, f: F) -> &mut Self
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                self.common.withf(f);
                self
            }

            $crate::expectation_methods!{}
        }

        }
        use $module::Expectation as $expectation;
    }
}
