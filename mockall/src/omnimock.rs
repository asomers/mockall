// vim: tw=80
/// # Arguments
///
/// * `i`:              Method's input type, usually a tuple like (u32, &i16)
/// * `imatch`:         
/// * `o`:              Output type.  Must be static
#[macro_export]
macro_rules! omnimock {
    ($i:ty, $ivar:ident, $imatch:ty, $imatchcall:expr, $o:ty) => {
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

        struct Matcher(::std::sync::Mutex<Box<Predicate<$imatch> + 'static>>);

        impl Matcher {
            fn new<P: ::mockall::Predicate<$imatch> + Send + 'static>(p: P) -> Self {
                Matcher(::std::sync::Mutex::new(Box::new(p)))
            }

            fn verify(&self, i: &$imatch) {
                let mut guard = self.0.lock().unwrap();
                if let Some(case) = guard.find_case(false, &i) {
                    let c = &case as &::predicates_tree::CaseTreeExt;
                    let tree = c.tree();
                    panic!("Expectation didn't match arguments:\n{}", tree);
                }
            }
        }

        impl Default for Matcher {
            fn default() -> Self {
                Matcher::new(::predicates::constant::always())
            }
        }

        #[derive(Default)]
        struct ExpectationCommon {
            matcher: Matcher,
            seq_handle: Option<::mockall::SeqHandle>,
            times: ::mockall::Times
        }

        impl ExpectationCommon {
            fn call(&self, i: &$imatch) {
                self.matcher.verify(i);
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

            fn with<P>(&mut self, p: P)
                where P: ::mockall::Predicate<$imatch> + Send + 'static
            {
                self.matcher = Matcher::new(p);
            }
        }

        #[derive(Default)]
        struct Expectation {
            common: ExpectationCommon,
            rfunc: ::std::sync::Mutex<Rfunc>,
        }

        impl Expectation {
            pub fn call(&self, $ivar: $i) -> $o {
                self.common.call($imatchcall);
                self.rfunc.lock().unwrap().call_mut($ivar)
            }
            
            pub fn returning<F>(&mut self, f: F) -> &mut Self
                where F: FnMut($i) -> $o + Send + 'static
            {
                {
                    let mut guard = self.rfunc.lock().unwrap();
                    ::std::mem::replace(guard.deref_mut(), Rfunc::Mut(Box::new(f)));
                }
                self
            }

            pub fn with<P>(&mut self, p: P) -> &mut Self
                where P: ::mockall::Predicate<$imatch> + Send + 'static
            {
                self.common.with(p);
                self
            }
        }

        //#[derive(Default)]
        //struct Expectations(Vec<Expectation>);

        //impl Expectations {
            //pub fn call(&self, i: $i) -> $o {
                //let n = self.0.len();
                //match self.0.iter().find(|e| e.matches(&i) &&
                                            //(!e.is_done() || n == 1))
                //{
                    //None => panic!("No matching expectation found"),
                    //Some(e) => e.$call(i)
                //}
            //}

            //pub fn expect(&mut self) -> &mut Expectation {
                //let e = Expectation::default();
                //self.0.push(e);
                //let l = self.0.len();
                //&mut self.0[l - 1]
            //}
        //}
    }
}

