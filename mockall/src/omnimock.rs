// vim: tw=80
/// # Arguments
///
/// * `i`:              Method's input type, usually a tuple like (u32, &i16)
/// * `o`:              Output type.  Must be static
/// * `ivar`:           Name of the call method's argument.  Ideally this
///                     wouldn't need to be specified, but it's required due to
///                     Rust macro hygiene rules.  Usually use "i".
/// * `imatchcall`:     Expression that produces a comma-delimited sequence of
///                     references from $ivar.
/// * `iargs`:          comma-delimited sequence of argument names for each
///                     argument
/// * `matchty`:        comma-delimited sequence of types for each match
///                     argument.  Must all be 'static
///
/// # Examples
///
/// ```
/// // Mock a method like foo(&self, x: u32, y: &i16) -> u32
/// omnimock!{(u32, &i16), u32, i, [&i.0, i.1], [i0, i1], [u32, i16]}
#[macro_export]
macro_rules! omnimock {
    ($i:ty, $o:ty, $ivar:ident,
        [ $( $matchcall:expr ),* ],
        [ $( $matcharg:ident ),* ],
        [ $( $predarg:ident ),* ],
        [ $( $matchty:ty ),* ]) =>
    {
        enum Rfunc {
            Default,
            // Indicates that a `return_once` expectation has already returned
            Expired,
            Mut(Box<dyn FnMut($i) -> $o + Send>),
            // Should be Box<dyn FnOnce> once that feature is stabilized
            // https://github.com/rust-lang/rust/issues/28796
            Once(Box<dyn FnMut($i) -> $o + Send>),
        }

        impl  Rfunc {
            fn call_mut(&mut self, args: $i) -> $o {
                match self {
                    Rfunc::Default => {
                        unimplemented!()
                        //Self::return_default()
                    },
                    Rfunc::Expired => {
                        panic!("Called a method twice that was expected only once")
                    },
                    Rfunc::Mut(f) => {
                        f(args)
                    },
                    Rfunc::Once(_) => {
                        let fo = ::std::mem::replace(self, Rfunc::Expired);
                        if let Rfunc::Once(mut f) = fo {
                            f(args)
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
            fn eval(&self, $( $matcharg: &$matchty, )*) {
                let passed = match self {
                    Matcher::Func(f) => f($( $matcharg, )*),
                    Matcher::Pred($( $predarg, )*) =>
                        [$( $predarg.eval($matcharg), )*]
                        .into_iter()
                        .all(|x| *x),
                };
                assert!(passed);
            }

            fn verify(&self, $( $matcharg: &$matchty, )* ) {
                self.eval( $( $matcharg, )* );
            }
        }

        impl Default for Matcher {
            #[allow(unused_variables)]
            fn default() -> Self {
                Matcher::Func(Box::new(|$( $matcharg, )*| true))
            }
        }

        #[derive(Default)]
        struct Common {
            matcher: ::std::sync::Mutex<Matcher>,
            seq_handle: Option<::mockall::SeqHandle>,
            times: ::mockall::Times
        }

        impl Common {
            fn call(&self, $( $matcharg: &$matchty, )* ) {
                self.matcher.lock().unwrap().verify($( $matcharg, )*);
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

            #[allow(non_camel_case_types)]  // Repurpose $predarg for generics
            pub fn with<$( $predarg: ::mockall::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $matcharg: $predarg,)*)
            {
                use ::std::ops::DerefMut;
                let mut guard = self.matcher.lock().unwrap();
                let m = Matcher::Pred($( Box::new($matcharg), )*);
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
            fn call(&self, $ivar: $i) -> $o {
                self.common.call($( $matchcall, )*);
                self.rfunc.lock().unwrap().call_mut($ivar)
            }

            pub fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($i) -> $o + Send + 'static
            {
                {
                    use ::std::ops::DerefMut;
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
                }
                self
            }

            #[allow(non_camel_case_types)]  // Repurpose $predarg for generics
            pub fn with<$( $predarg: ::mockall::Predicate<$matchty> + 'static,)*>
                (&mut self, $( $matcharg: $predarg,)*) -> &mut Self
            {
                self.common.with($( $matcharg, )*);
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
