// vim: tw=80
/// # Arguments
///
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
/// ```
/// // Mock a method like foo(&self, x: u32, y: &i16) -> u32
/// omnimock!{(u32, &i16), u32, i, [&i.0, i.1], [i0, i1], [u32, i16]}
#[macro_export]
macro_rules! omnimock {
        ($o:ty,
        [ $( $methty:ty ),* ],
        [ $( $matchcall:expr ),* ],
        [ $( $args:ident ),* ],
        [ $( $predargs:ident ),* ],
        [ $( $matchty:ty ),* ]) =>
    {
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
            Pred( $( Box<::mockall::Predicate<$matchty>>, )* )
        }

        impl Matcher {
            fn eval(&self, $( $args: &$matchty, )*) {
                let passed = match self {
                    Matcher::Func(f) => f($( $args, )*),
                    Matcher::Pred($( $predargs, )*) =>
                        [$( $predargs.eval($args), )*]
                        .into_iter()
                        .all(|x| *x),
                };
                assert!(passed);
            }

            fn verify(&self, $( $args: &$matchty, )* ) {
                self.eval( $( $args, )* );
            }
        }

        impl Default for Matcher {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|$( $args, )*| true))
            }
        }

        #[derive(Default)]
        struct Common {
            matcher: ::std::sync::Mutex<Matcher>,
            seq_handle: Option<::mockall::SeqHandle>,
            times: ::mockall::Times
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

            fn satisfy_sequence(&self) {
                if let Some(handle) = &self.seq_handle {
                    handle.satisfy()
                }
            }

            fn verify_sequence(&self) {
                if let Some(handle) = &self.seq_handle {
                    handle.verify()
                }
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            pub fn with<$( $predargs: ::mockall::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*)
            {
                use ::std::ops::DerefMut;
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred($( Box::new($args), )*);
                ::std::mem::replace(guard.deref_mut(), m);
            }

            fn withf<F>(&mut self, f: F)
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                use ::std::ops::DerefMut;
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Func(Box::new(f));
                ::std::mem::replace(guard.deref_mut(), m);
            }
        }

        #[derive(Default)]
        struct Expectation {
            common: Common,
            rfunc: ::std::sync::Mutex<Rfunc>,
        }

        impl Expectation {
            fn call(&self, $( $args: $methty, )* ) -> $o {
                self.common.call($( $matchcall, )*);
                self.rfunc.lock().unwrap().call_mut($( $args, )*)
            }

            pub fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($( $methty, )*) -> $o + Send + 'static
            {
                {
                    use ::std::ops::DerefMut;
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
                }
                self
            }

            #[allow(non_camel_case_types)]  // Repurpose $predargs for generics
            pub fn with<$( $predargs: ::mockall::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $args: $predargs,)*) -> &mut Self
            {
                self.common.with($( $args, )*);
                self
            }

            fn withf<F>(&mut self, f: F) -> &mut Self
                where F: Fn($( &$matchty, )* ) -> bool + Send + 'static
            {
                self.common.withf(f);
                self
            }
        }
    }
}
