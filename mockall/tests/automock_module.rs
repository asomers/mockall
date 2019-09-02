// vim: tw=80
//! Mocking an entire module of functions

// mocking modules requires the proc_macro_hygiene feature in the _consumer_
// code
#![cfg_attr(feature = "nightly", feature(proc_macro_hygiene))]

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(feature = "nightly")] {
        mod m {
            use mockall::*;

            #[automock]
            #[allow(unused)]
            mod foo {
                pub fn bar(_x: u32) -> i64 {unimplemented!()}
            }

            #[test]
            #[should_panic(expected = "mock_foo::bar: No matching expectation found")]
            fn with_no_matches() {
                let ctx = mock_foo::bar_context();
                ctx.expect()
                    .with(predicate::eq(4))
                    .return_const(0);
                mock_foo::bar(5);
            }

            #[test]
            fn returning() {
                let ctx = mock_foo::bar_context();
                ctx.expect()
                    .returning(|x| i64::from(x) + 1);
                assert_eq!(5, mock_foo::bar(4));
            }
        }
    }
}
