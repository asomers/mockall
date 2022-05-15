# mockall_double

A double test adapter that works well with Mockall.

[![Build Status](https://api.cirrus-ci.com/github/asomers/mockall.svg)](https://cirrus-ci.com/github/asomers/mockall)
[![Crates.io](https://img.shields.io/crates/v/mockall_double.svg)](https://crates.io/crates/mockall_double)
[![Documentation](https://docs.rs/mockall_double/badge.svg)](https://docs.rs/mockall_double)

## Overview

Mockall can easily create a mock version of a struct.  But how does one
convince the code under test to use the mock struct instead of the real one?
In Rust, it's necessary to replace the real struct at compile time.  That's
very easy to do with a few `#[cfg(test)]` statements.  But mockall_double makes
it even easier.

## Usage

Typically `mockall` is only used by unit tests, so it can be a dev-dependency.
But `mockall_double` must be a full dependency.  To use it this way, add this to
your `Cargo.toml`:

```toml
[dependencies]
mockall_double = "0.2.1"

[dev-dependencies]
mockall = "0.11.0"
```

Then use it like this:

```rust
use mockall_double::double;

mod mockable {
    #[cfg(test)]
    use mockall::automock;

    pub struct Foo {}
    #[cfg_attr(test, automock)]
    impl Foo {
        pub fn foo(&self, x: u32) -> u32 {
            // ...
            0
        }
    }
}

#[double]
use mockable::Foo;

fn bar(f: Foo) -> u32 {
    f.foo(42)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bar_test() {
        let mut mock = Foo::new();
        mock.expect_foo()
            .returning(|x| x + 1);
        assert_eq!(43, bar(mock));
    }
}
```

See the [API docs](https://docs.rs/mockall_double) for more information.

# Minimum Supported Rust Version (MSRV)

`mockall_double` is tested with the same MSRV as Mockall itself.  Currently,
that's Rust 1.42.0.  `mockall_double`'s MSRV will not be changed in the future
without bumping the major or minor version.

# License

`mockall_double` is primarily distributed under the terms of both the MIT
license and the Apache License (Version 2.0).

See LICENSE-APACHE, and LICENSE-MIT for details

# Acknowledgements

`mockall_double` is inspired by Jason Grlicky's
[double](https://crates.io/crates/double) crate, but tweaked to work better
with Mockall's naming conventions.
