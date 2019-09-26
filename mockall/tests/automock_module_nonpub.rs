// vim: tw=80
//! bare functions can use non-public types, as long as the object's visibility is compatible.

// mocking modules requires the proc_macro_hygiene feature in the _consumer_
// code
#![cfg_attr(feature = "nightly", feature(proc_macro_hygiene))]

#[allow(unused)]
cfg_if::cfg_if! {
    if #[cfg(feature = "nightly")] {
        mod outer {
            struct SuperT();

            mod inner {
                use mockall::automock;

                pub(crate) struct PubCrateT();
                struct PrivT();

                #[automock]
                mod m {
                    use super::*;

                    fn foo(x: PubCrateT) -> PubCrateT { unimplemented!() }
                    fn bar(x: PrivT) -> PrivT { unimplemented!() }
                    fn baz(x: super::super::SuperT) -> super::super::SuperT {
                        unimplemented!()
                    }
                    fn bang(x: crate::outer::SuperT) -> crate::outer::SuperT {
                        unimplemented!()
                    }
                }
            }
        }
    }
}
