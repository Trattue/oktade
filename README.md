# oktade

Parser for JVM classfiles, built apon the parser combinator library
[attoparsec](https://hackage.haskell.org/package/attoparsec).

This software is still in early development and not ready fo serious usage yet.

Oktade is able to parse entire classfiles correctly, however some things still
need to be done:
* Argument parsing (including the bytecode parsing). Currently, all arguments
  are represented as bvytestrings in their most general form possible.
* Signature string parsing. Currently, signatures are stored as bytestrings.
* MUTF-8 decoding and encoding.
* Write tests.
* Implement operations modifying the classfile/bytecode.
* Profile and improve performace, if necessary.
* Learn more Haskell and improve code quality.

## Usage

The main executable can be used in a similar fashion to `javap`: Run
`./oktade <classfile>` to have the parsed classfile printed out.

## Development
### Prerequisites

A working installation of Haskell (GHC 8.10.5) and cabal. The easiest way of
achieving that is using [GHCup](https://www.haskell.org/ghcup/).

### Development Environment Setup

Clone this project via `git clone https://github.com/Trattue/oktade` or using
your git client.

### Building and Testing

Run `cabal build` to produce executables of all targets. This will produce
executables of the main application, the test application and the benchmark
application.

For testing, run `cabal test` afterwards.

For benchmarking, run `cabal bench` afterwards.

## License

Copyright 2021 Trattue

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
