# oktade

Parser for JVM classfiles, built upon the parser combinator library
[attoparsec](https://hackage.haskell.org/package/attoparsec).

This software is still in early development and not ready fo serious usage yet.

Oktade is able to parse entire classfiles correctly, however some things still
need to be done:
* Argument parsing (including the bytecode parsing). Currently, all arguments
  are represented as bytestrings in their most general form possible.
* Signature string parsing. Currently, signatures are stored as bytestrings.
* MUTF-8 decoding and encoding.
* Write tests.
* Implement operations modifying the classfile/bytecode.
* Profile and improve performance, if necessary.
* Learn more Haskell and improve code quality.

## Usage

The main executable can be used in a similar fashion to `javap`: Run
`./oktade <classfile>` to have the parsed classfile printed out.

## Development
### Prerequisites

A working installation of Haskell (GHC 9.0.1) and cabal (3.6.2.0). The easiest
way of obtaining those is using [GHCup](https://www.haskell.org/ghcup/).

### Development Environment Setup

Clone this project via `git clone https://github.com/Trattue/oktade` or using
your git client.

### Building and Testing

Run `cabal build` to produce executables of all targets. This will produce
executables of the main application, the test application and the benchmark
application.

For testing, run `cabal test` afterwards.

For benchmarking, run `cabal bench` afterwards.

### Code Style

Use the [ormolu formatter](https://github.com/tweag/ormolu); maximum line length
is 80 (except for stuff like URIs). Make sure to follow all of
[hlint](https://github.com/ndmitchell/hlint)'s suggestions.

For the [.cabal file](oktade.cabal), use `cabal format`.

### Dependency Management

Always try using the latest dependencies.

Run `cabal gen-bounds` to generate suitable version bounds for dependencies and
copy those to [oktade.cabal](oktade.cabal). If `cabal outdated` reports any
outdated dependencies, adjust the versions in [oktade.cabal](oktade.cabal),
check if the project still builds and works correctly, then run
`cabal gen-bounds` again and copy those versions to
[oktade.cabal](oktade.cabal).

## License

Copyright (c) 2021 Trattue

Oktade is distributed under the terms of the Apache License (Version 2.0), see
[LICENSE](LICENSE) for details.

The oktade repository includes code from other projects which may be licensed
under other terms and conditions:
* [cfr_tests](https://github.com/leibnitz27/cfr_tests): Licensed under the MIT
  License license (see [tests/LICENSE](tests/LICENSE) for details).<br>
  Compiled classes from the project can be found in the [tests/](tests/)
  directory.<br>
  Thanks to the [CFR](https://github.com/leibnitz27/cfr) developers for making
  their comprehensive test suite open source!
