// vim: tw=80

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
