# prettyless

[![build](https://github.com/typstyle-rs/prettyless/actions/workflows/ci.yml/badge.svg)](https://github.com/typstyle-rs/prettyless/actions/workflows/ci.yml)
[![Docs](https://docs.rs/prettyless/badge.svg)](https://docs.rs/prettyless)

Pretty printing combinators for Rust

This is an unofficial forked version of [pretty.rs](https://github.com/Marwes/pretty.rs) by @Marwes, branched from v0.12.4. We extend it with more features used in [typstyle](https://github.com/typstyle-rs/typstyle), a code formatter for [Typst](https://typst.app/). The correctness of modifications are partly guaranteed by the testsuite of typstyle.

## Synopsis

This crate provides functionality for defining pretty printers. It is
particularly useful for printing structured recursive data like trees.

The implementation was originally based on Larsen's SML translation
(https://github.com/kfl/wpp) of Wadler's Haskell pretty printer
(https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf). It
has since been modified in various ways to better fit Rust's
programming model. In particular, it uses iteration rather than
recursion and provides streaming output.

## Documentation

See the generated API documentation [here](https://docs.rs/pretty).

## Requirements

1. [Rust](https://www.rust-lang.org/)
2. [Cargo](https://crates.io/)

You can install both with the following:

```bash
curl -s https://static.rust-lang.org/rustup.sh | sudo sh
```

See [Installation](https://doc.rust-lang.org/book/ch01-01-installation.html) for further details.

## Usage

```bash
cargo build                                          ## build library and binary
cargo run --example trees                            ## run the example (pretty trees)
cargo bench                                          ## run benchmarks
cargo test                                           ## run tests
```
