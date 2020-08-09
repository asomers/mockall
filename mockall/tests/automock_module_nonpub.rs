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
                #[allow(unused)]
                mod m {
                    use super::*;

                    pub(crate) fn foo(x: PubCrateT) -> PubCrateT {
                        unimplemented!()
                    }
                    pub(super) fn bar(x: PrivT) -> PrivT {
                        unimplemented!()
                    }
                    pub(in super::super) fn baz(x: super::super::SuperT)
                        -> super::super::SuperT
                    {
                        unimplemented!()
                    }
                    pub(in crate::outer) fn bang(x: crate::outer::SuperT)
                        -> crate::outer::SuperT
                    {
                        unimplemented!()
                    }
                }

                #[test]
                fn returning() {
                    let ctx = mock_m::foo_context();
                    ctx.expect()
                        .returning(|x| x);
                    mock_m::foo(PubCrateT());
                }
            }
        }
    }
}
